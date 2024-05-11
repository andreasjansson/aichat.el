(require 'ert)
(require 'parsec)
(require 'cl-lib)

(ert-deftest test-parse-dialog ()
  (let ((temp-file (make-temp-file "hello-world")))
    (with-temp-file temp-file
      (insert "hello world"))

    (should
     (equal
      (aichat--parse-dialog (concat "u1

## ASSISTANT:

a1 <!-- comment -->

```
## USER:
foo
```

## SYSTEM:
s1 <!-- comment -->..

## USER:
u2
<ai-context>" temp-file "</ai-context>
"))
      '((user . "u1")
        (assistant . "a1 <!-- comment -->

```
## USER:
foo
```")
        (system . "s1 ..")
        (user . "u2
hello world"))))

    (should
     (equal
      (aichat--parse-dialog "

## USER:
u1
## ASSISTANT:
a1
")
      '((user . "u1")
        (assistant . "a1"))))

    (delete-file temp-file)))
