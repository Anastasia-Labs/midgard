## Single Operator Processing Steps & Logic

Currently, "phase 1" validation consists of successful deserialization of the
payload into a `CML.Transaction` value, and "phase 2" is always true.

1. Received transactions go through phase 1 validation and if they pass, they'll
   be put in the queue for further processing.
2. The transaction queue is processed in batches, and any transaction that
   passes phase 2 validation will be added to `MempoolDB`.
3. Concurrently the user events tables (i.e. withdrawal, transaction orders, and
   deposits) are populated periodically by querying their addresses on Cardano.
   Withdrawals and transaction orders will only be added to their respective
   tables if they pass their verification checks (for transaction orders those
   will be phase 1 and phase 2 validations, but withdrawals will need to have
   their own dedicated checks).
4. Commitment worker runs periodically:
   a) Reads the latest unsubmitted block from `BlocksDB` and stores it in
      memory as `latestBlock`.
   b) Stores the current time and only then retrieves all entries from
      `MempoolDB`:
      - If no entries existed (empty mempool), uses the stored current time as
        the upper bound of the block.
      - Otherwise, uses the timestampt of the newest mempool entry as block's
        uppser bound.
      At this point, previously commited block is available as `latestBlock`,
      and the upper bound of the block about to be committed is established.
   c) Using `latestBlock`'s upper bound and the newly established one, retrieves
      all user events falling within the time window. At this point, all
      withdrawals, transaction orders and requests, and deposits that should be
      included in the block are available.
   d) Retrieves the ledger Merkle Patricia Trie (MPT) from disk. This _should
      be_ the state of Midgard ledger after `latestBlock` (TODO, some syncing
      mechanism might be needed).
   e) Applies events to the ledger in order:
          i. Withdrawals
         ii. Transaction orders
        iii. Transaction requests
         iv. Deposits
      Within each category, events are sorted by their time stamps. However,
      this should only matter for transactions.
   f) Find the roots of withdrawals and deposits (mostly likely during their
      application to the ledger). The ledger's root is already calculated.
   g) Uses the 3 MPT roots, `latestBlock`, and the new event interval to build
      the new block.
   h) Switches to the operator's dedicated block commitment Cardano wallet.
   i) Retrieves latest state of wallet and contract UTxOs from the blocks table,
      overrides the wallet's UTxO in LE's interface, builds the block commitment
      transaction and signs it.
   j) Adds another entry to the blocks table, with the status flag set to
      "unsubmitted," containing the unsubmitted transaction CBOR along with the
      wallet's state after the transaction, and its produced UTxOs.
   All these steps are to be performed in such a way that any failure would lead
   to reversal of any changes to the ledger MPT.
5. Block submission fiber runs periodically:
   a) Retrieves the oldest unsubmitted block from the blocks table and submits
      its signed Cardano transaction. Fiber dies if this fails. (TODO, we
      should implement a recovery mechanism in case the already built and signed
      transaction had become invalid).
   b) Retrieves all transaction requests which their timestampts fall within the
      submitted block's event interval from `MempoolDB`.
   c) Similarly, retrieves all user events from its 3 tables.
   d) Retrieves all entries of `LatestLedgerDB`. This _should be_ the state of
      latest submitted block commitment.
   e) Similar to ledger MPT, applies events to `LatestLedgerDB` in the same
      order, and updates the table.
   f) Transfers included mempool transactions to `ImmutableDB`.
   g) Marks submitted block as "submitted."
