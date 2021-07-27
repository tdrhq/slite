FROM debian:stable

RUN apt-get update && apt-get install -y sbcl
RUN adduser -q jenkins
WORKDIR /home/jenkins

ADD https://beta.quicklisp.org/quicklisp.lisp quicklisp.lisp
RUN chmod a+r quicklisp.lisp
