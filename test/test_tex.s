% -*- mode:latex -*-
@x output_tex
\documentclass[a4paper,11pt]{article}

\usepackage{hyperref}

\begin{document}

This is a little test for Latex:

@o prova.c
@{
@<includes@>

@<main function@>
@}

In C we should insert the include in another section:

@d includes
@{
#include <stdio.h>
@}

The main function is here:

@d main function
@{
int main(int argc, char **argv) {
  printf("Ciao da me");
  return 0;
}
@}

\end{document}
