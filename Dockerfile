FROM debian:stable

RUN apt-get update && apt-get install -y sbcl git-core
