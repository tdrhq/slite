load("//tools:lisp.bzl", "lisp_library", "lisp_test")

lisp_library(
    name = "slite",
    srcs = [
        "api.lisp",
        "slite.lisp",
        "fiveam.lisp",
    ],
    deps = [
        "//quicklisp:str",
        "//quicklisp:fiveam",
    ],
    visibility = [
        "PUBLIC",
    ],
)

lisp_library(
    name = "parachute",
    srcs = [
        "parachute.lisp",
    ],
    deps = [
        ":slite",
        "//quicklisp:parachute",
    ],
)

lisp_test(
    name = "tests",
    deps = [
        ":slite",
        ":parachute",
    ],
    srcs = [
        "test-slite.lisp",
        "test-parachute.lisp",
    ],
)
