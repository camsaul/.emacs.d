;;; -*- lexical-binding: t; -*-

(require 'guide-key)

(setq guide-key/idle-delay                  1.0
      guide-key/recursive-key-sequence-flag t
      guide-key/guide-key-sequence          '("<f12>"
                                              "<f1>"
                                              "<help>"
                                              "A-'"
                                              "A-*"
                                              "A-,"
                                              "A-/"
                                              "A-1"
                                              "A-3"
                                              "A-\""
                                              "A-^"
                                              "A-_"
                                              "A-`"
                                              "A-r"
                                              "A-~"
                                              "C-c"
                                              "C-h"
                                              "C-x"
                                              "M-o"))

(provide 'cam-guide-key)
