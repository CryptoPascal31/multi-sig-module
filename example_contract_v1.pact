(namespace 'free)
(module my-sc GOVERNANCE
  (defconst VERSION 1)

  (defcap GOVERNANCE ()
    (free.multi-sig-manager.enforce-approved "MY-DOMAIN"))
)
