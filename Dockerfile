FROM debian:stable

RUN apt-get update && apt-get install -y sbcl git-core
ADD https://beta.quicklisp.org/quicklisp.lisp /quicklisp.lisp
RUN mkdir /cache-dir
RUN HOME=/cache-dir && sbcl --load quicklisp.lisp --eval '(quicklisp-quickstart:install)  '
RUN env
RUN ls -l /cache-dir
RUN chmod -R a+rwx /cache-dir
