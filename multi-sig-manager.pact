(module multi-sig-manager GOVERNANCE
  (use util.guards1)

  (defcap GOVERNANCE ()
    ; Should be not upgradable but for tests it's OK
    (enforce-keyset "user.multi-sig-admin"))

  (defschema domain-sch
    domain:string
    signers:[guard]
    required-signers:integer
  )

  (defschema transaction-sch
    trx-hash:string
    domain:string
    approvals-count:integer
  )

  (defschema approval-sch
    approved:bool
  )

  (deftable domains:{domain-sch})

  (deftable transactions:{transaction-sch})

  (deftable approvals:{approval-sch})

  (defun enforce-valid-signers:bool (signers:[guard] required-signers:integer)
    ; Check required signers
    (enforce (and? (< 0) (>= (length signers)) required-signers) "Required signers count is bad")

    ; Check for duplicates
    (enforce (= (length signers) (length (distinct signers))) "Duplicates in guards")
  )


  (defun add-domain:string (domain:string signers:[guard] required-signers:integer)
    @doc "Create a new signature domain"
    (enforce-valid-signers signers required-signers)
    (insert domains domain {'domain:domain,
                            'signers:signers,
                            'required-signers:required-signers})
  )


  (defun rotate-domain:string (domain:string signers:[guard] required-signers:integer)
    @doc "Rotate an existing domain"
    (enforce-approved domain)
    (enforce-valid-signers signers required-signers)
    (update domains domain {'signers:signers,
                            'required-signers:required-signers})
  )


  (defun propose-transaction:string (domain:string trx-hash:string)
    @doc "First step => Someone propose a transaction"
    ; Verify that the proposer is a signer of the domain
    (with-read domains domain {'signers:=domain-signers}
      (enforce-guard-any domain-signers))

    (insert transactions trx-hash {'trx-hash:trx-hash,
                                  'domain:domain,
                                  'approvals-count:0})
  )

  (defun approve:string (trx-hash:string approver-guard:guard)
    @doc "Second step => Approvers approve"
    ; First enforce the guard
    (enforce-guard approver-guard)

    ; Ensure that this approval is not a duplicate
    (insert approvals (hash (+ trx-hash (hash approver-guard))) {'approved:true})

    (with-read transactions trx-hash {'domain:=domain, 'approvals-count:=old-approvals-count}
      ; Check that the signer is allowed for this domain
      (with-read domains domain {'signers:=signers}
        (enforce (contains approver-guard signers) "Guard is not allowed to sign this transaction"))

      ; Update the approvals-count
      (update transactions trx-hash {'approvals-count: (+ 1 old-approvals-count)}))
  )

  (defun enforce-approved:bool (domain:string)
    @doc "Third step => Include this call in module GOVERNANCE"
    (with-read domains domain {'required-signers:=required-signers}
      (with-read transactions (tx-hash) {'approvals-count:=approvals-count, 'domain:=tx-domain}
        (enforce (= domain tx-domain) "Bad domain name")
        (enforce (>= approvals-count required-signers) "Not enough approvals")))
  )

  (defun get-approved-guard:guard (domain:string)
    @doc "Guard can be used for account or namespace"
    (create-user-guard (enforce-approved domain)))

)
