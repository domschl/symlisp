import lldb
import sys

# --- Configuration ---
# The bad pointer value seen in the crash (sl_gc_mark argument)
# IMPORTANT: Update this if the crash address changes!
bad_ptr_val = 0x600050080
# --- End Configuration ---

debugger = lldb.debugger
target = debugger.GetSelectedTarget()
process = target.GetProcess()
thread = process.GetSelectedThread()
frame = thread.GetSelectedFrame()

# Find the frame where sl_gc is running (usually one frame up from sl_gc_mark)
gc_frame = None
for f in thread:
    if f.GetFunctionName() == "sl_gc":
        gc_frame = f
        break

if not gc_frame:
    print("Error: Could not find the 'sl_gc' stack frame.", file=sys.stderr)
    # Optionally try the current frame if sl_gc_mark was inlined or structure changed
    # gc_frame = frame
    # if not gc_frame or gc_frame.GetFunctionName() != "sl_gc_mark": # Basic check
    #    print("Error: Current frame is not sl_gc_mark either.", file=sys.stderr)
    #    sys.exit(1)
    sys.exit(1)


print(f"Inspecting GC state in frame: {gc_frame.GetFunctionName()}")

root_count_val_expr = gc_frame.EvaluateExpression("root_count")
gc_roots_addr_expr = gc_frame.EvaluateExpression("gc_roots")
ptr_size_expr = gc_frame.EvaluateExpression("sizeof(sl_object**)")

if not root_count_val_expr.IsValid() or not gc_roots_addr_expr.IsValid() or not ptr_size_expr.IsValid():
    print("Error: Could not evaluate GC variables (root_count, gc_roots, sizeof). Are symbols loaded?", file=sys.stderr)
    sys.exit(1)

root_count_val = root_count_val_expr.GetValueAsUnsigned()
gc_roots_addr = gc_roots_addr_expr.GetValueAsUnsigned()
ptr_size = ptr_size_expr.GetValueAsUnsigned()

print(f"Searching {root_count_val} roots starting at 0x{gc_roots_addr:x}")
found = False

for i in range(root_count_val):
    root_var_addr_addr = gc_roots_addr + i * ptr_size
    err = lldb.SBError()

    # Read the sl_object** (address of the stack variable) from gc_roots[i]
    root_var_addr = process.ReadPointerFromMemory(root_var_addr_addr, err)
    if not err.Success() or root_var_addr == 0:
        print(f"Error or NULL reading gc_roots[{i}] address at 0x{root_var_addr_addr:x}")
        continue

    # Now read the sl_object* (the actual object pointer) stored AT root_var_addr
    obj_ptr = process.ReadPointerFromMemory(root_var_addr, err)
    if not err.Success():
        # This is the key indicator of a dangling stack pointer in gc_roots
        print(f"----> Error reading object pointer from root variable address 0x{root_var_addr:x} (gc_roots[{i}])")
        print(f"----> This strongly suggests gc_roots[{i}] (0x{root_var_addr:x}) is a DANGLING STACK POINTER!")
        lookup_cmd = f"image lookup -a 0x{root_var_addr:x}"
        print(f"----> Run in LLDB: {lookup_cmd}")
        # We might not find the bad_ptr_val directly if the read fails, but the dangling root is the cause.
        found = True # Mark as found because we identified the dangling root address
        # break # Optional: Stop after first dangling pointer found
        continue # Continue searching other roots

    print(f"gc_roots[{i}] = 0x{root_var_addr:x} -> *0x{root_var_addr:x} = 0x{obj_ptr:x}")

    if obj_ptr == bad_ptr_val:
        print(f"----> Found bad object pointer 0x{obj_ptr:x} at index {i}")
        print(f"----> The responsible stack variable address is: 0x{root_var_addr:x}")
        lookup_cmd = f"image lookup -a 0x{root_var_addr:x}"
        print(f"----> Run in LLDB: {lookup_cmd}")
        found = True
        break # Stop after finding the specific bad pointer

if not found:
    print(f"Could not find the specific bad pointer 0x{bad_ptr_val:x} by direct value comparison, but check for dangling stack pointers reported above.")
