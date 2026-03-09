## Single Operator Processing Steps & Logic

Currently, "phase 1" validation consists of successful deserialization of the
payload into a `CML.Transaction` value, and "phase 2" is always true.

1. Received transactions go through phase 1 validation and if they pass, they'll
   be put in the queue for further processing.
2. The transaction queue is processed in batches, and any transaction that
   passes phase 2 validation will be added to `MempoolDB`, while also updating
   `MempoolLedgerDB` and `AddressHistoryDB` accordingly (added entries to
   `AddressHistoryDB` will be marked as "slated").
3. Concurrently the user events tables (i.e. withdrawal, transaction orders, and
   deposits) are populated periodically by querying their addresses on Cardano.
   Withdrawals and transaction orders will only be added to their respective
   tables if they pass their verification checks (for transaction orders those
   will be phase 1 and phase 2 validations, but withdrawals will need to have
   their own dedicated checks).
4. Commitment worker runs periodically:
   a) Reads the latest block from `BlocksDB` and stores it in memory as
      `latestBlock`.
   b) Stores the current time to use as the upper bound of the block's event
      interval.
   c) Retrieves all user events and transaction requests that fall within the
      established event interval. At this point, all withdrawals, transaction
      orders and requests, and deposits that should be included in the block are
      available (Note that transaction requests don't have formally recognized
      "inclusion times" as user events do. Their timestamps in `MempoolDB`
      should be used instead).
   d) Retrieves the ledger Merkle Patricia Trie (MPT) from disk. This _should
      be_ the state of Midgard ledger after `latestBlock` (TODO, some syncing
      mechanism might be needed).
   e) Applies events to the ledger in order:
          i. Withdrawals
         ii. Transaction orders
        iii. Transaction requests
         iv. Deposits
      Within each category, events are sorted by their timestamps. However, this
      should only matter for transactions and withdrawals.
   f) Find the roots of withdrawals and deposits (most likely during their
      application to the ledger). The ledger's root is found at this point.
   g) Uses the 3 MPT roots, `latestBlock`, and the new event interval to build
      the new block.
   h) Switches to the operator's dedicated block commitment Cardano wallet.
   i) Retrieves latest state of wallet and contract UTxOs from `BlocksDB`,
      overrides the wallet's UTxO in LE's interface, builds the block commitment
      transaction and signs it.
   j) Adds another entry to the blocks table, with the status flag set to
      "unsubmitted," containing the unsubmitted transaction CBOR along with the
      wallet's state after the transaction, and its produced UTxOs.
   All these steps are to be performed in such a way that any failure would lead
   to reversal of any changes to the ledger MPT.
5. Block submission fiber runs periodically:
   a) Retrieves the oldest unsubmitted block from `BlocksDB` and submits its
      signed Cardano transaction. Fiber dies if this fails. (TODO, we should
      implement a recovery mechanism in case the already built and signed
      transaction becomes invalid).
   b) Retrieves all transaction requests which their timestamps fall within the
      submitted block's event interval from `MempoolDB`.
   c) Similarly, retrieves all user events from its 3 tables.
   d) Processes all the retrieved events to find the ledger entries to be
      removed/added from/to `LatestLedgerDB`, and also entries to be added to
      `AddressHistoryDB`.
   e) Transfers included mempool transactions to `ImmutableDB` and
      `BlocksTxsDB`.
   g) Marks submitted block as "submitted."
