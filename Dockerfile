FROM debian:stable

RUN apt-get update && apt-get install -y sbcl

ADD https://beta.quicklisp.org/quicklisp.lisp

RUN sbcl --load quicklisp.lisp --eval '(quicklisp-quickstart:install)'