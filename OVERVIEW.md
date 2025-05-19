# SymLisp Project Overview

## 1. Introduction

SymLisp is a lightweight, embeddable Scheme-like interpreter with a strong focus on symbolic computation capabilities.

Architecturally, SymLisp combines a core interpreter logic implemented in C for performance, a rich standard library and symbolic algebra system written in its own Scheme dialect, and a Python-based Jupyter kernel for interactive notebook environments.

## 2. Core Philosophy & Goals

The SymLisp project aims to:

*   Provide a robust and usable Scheme-like environment for general programming tasks.
*   Offer powerful symbolic computation features, including algebraic expression simplification and expansion.
*   Be lightweight and easily embeddable within larger applications.
*   Support extensibility through its Scheme-based standard library and the potential for user-defined extensions.
*   Enable interactive usage through both a traditional Read-Eval-Print Loop (REPL) and an advanced Jupyter kernel interface.

## 3. Key Features

*   **Scheme-like Core:** Implements a significant subset of Scheme syntax and core procedures.
*   **Advanced Numerics:** Supports rational numbers and arbitrary-precision integers via the GNU Multiple Precision Arithmetic Library (GMP).
*   **Symbolic Computation:** Provides capabilities for symbolic manipulation of expressions, including simplification and expansion of algebraic and trigonometric terms.
*   **UTF-8 Support:** Handles strings and symbols with UTF-8 encoding.
*   **Interactive REPL:** Offers a command-line Read-Eval-Print Loop for direct interaction.
*   **Jupyter Kernel Integration:** Includes a Jupyter kernel allowing for use in interactive notebooks, with support for Markdown and LaTeX output of symbolic expressions.
*   **Scheme Standard Library:** Features a standard library written in Scheme, offering utilities for list processing, infix expression parsing, and further symbolic operations.

## 4. Target Audience

SymLisp is suitable for developers seeking an embeddable Scheme interpreter for their applications, students and educators exploring concepts in symbolic computation and programming language design, or researchers and hobbyists who require a customizable and interactive symbolic mathematics environment.
