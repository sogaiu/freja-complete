(-> ["janet"
     "./janet-usages-as-tests/janet-usages-as-tests/make-and-run-tests.janet"
     # specify file and/or directory paths relative to project root
     "./freja-complete/utils.janet"
     ]
    (os/execute :p)
    os/exit)

