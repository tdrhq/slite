FROM debian:stable

RUN apt-get update && apt-get install -y sbcl git-core
ADD https://beta.quicklisp.org/quicklisp.lisp /quicklisp.lisp
RUN chmod a+r quicklisp.lisp
