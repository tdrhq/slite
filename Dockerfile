FROM fukamachi/roswell:20.01.14.104-alpine

RUN touch foo
RUN apk add git
