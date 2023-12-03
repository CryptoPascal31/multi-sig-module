(namespace 'free)
(module my-sc GOVERNANCE
  (defconst VERSION 2)

  (defcap GOVERNANCE ()
    (free.multi-sig-manager.enforce-approved "MY-DOMAIN"))
)
