FROM debian:stable

RUN apt-get update && apt-get install -y sbcl
RUN adduser jenkins
USER jenkins

ADD https://beta.quicklisp.org/quicklisp.lisp quicklisp.lisp
RUN sbcl --load quicklisp.lisp --eval '(quicklisp-quickstart:install)'
