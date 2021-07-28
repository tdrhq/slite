FROM debian:stable

RUN apt-get update && apt-get install -y sbcl git-core
ADD https://beta.quicklisp.org/quicklisp.lisp /quicklisp.lisp
RUN sbcl --load quicklisp.lisp --eval '(quicklisp-quickstart:install)'
RUN chmod -R a+rwx /root/quicklisp
