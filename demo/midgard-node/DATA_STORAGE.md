## Relational Tables

We currently have the following tables:
- AddressHistoryDB
- BlocksDB
- BlocksTxsDB
- ConfirmedLedgerDB
- LatestLedgerDB
- MempoolLedgerDB
- ImmutableDB
- MempoolDB
- DepositsDB
- TxOrdersDB
- WithdrawalsDB

Apart from `AddressHistoryDB`, `BlocksDB`, and `BlocksTxsDB`, the rest are
instances of three abstract tables:
- Ledger
- Transactions
- User events

### Address History

For storing events and their corresponding addresses involved. Without this
table, querying interaction history of a given address would be very expensive.

This table maps "event IDs" to addresses, with 2 flags: one for the state of a
given record, and another for specifying the nature of its event.

Addresses can send/receive funds via 3 different interactions:
1. Transactions (either requests or orders)
2. Deposits
3. Withdrawals

Each entry can also have 4 different states:
1. Slated: For events that are not included in a block commitment. This applies
   to transactions in the mempool, and user events with inclusion times in
   future.
2. Committed: For events that are included in a committed block. Committed
   blocks are the ones stored in `BlocksDB` which are not yet submitted.
3. Submitted: For events that are included in a submitted block commitment.
4. Confirmed: For events included in a block that's been merged into the
   confirmed state.

To add an entry based on a given transaction, first the inputs and outputs of
the transaction must be extracted. Next, the spent inputs must be resolved using
the most up-to-date ledger (which is `MempoolLedgerDB`) so that involved
addresses can be specified. The "event ID" will be the hash of the given
transaction.

For withdrawals, similar to transactions, first their spent input must be
resolved to extract the involved address. For "event ID," the withdrawal ID is
used. The primary difference in resolution is that while it too looks up
`MempoolLedgerDB`, the entry must not be recorded if the spent UTxO is still
non-existent.

Deposits are simpler as they don't need resolving. Their involved addresses are
extracted from their "event info" field.

It is of note that we are currently treating transaction requests and orders
identically. That is, while the event ID of a transaction order is different
than the hash of the transaction they carry, in this table we are ignoring their
event IDs and simply recording their transaction hashes as their identifiers.

### Blocks

Stores various information for individual block commitments. It primarily maps
header hashes to signed L1 transactions (appending to the state queue). Since
we are not submitting the transactions, the wallet state, along with the
produced UTxOs in each transaction need to be explicitly recorded in each row,
so that the consequent block commitments can be built without waiting for
on-chain confirmation.

Similar to `AddressHistoryDB`, each entry here is also marked with a status.
Namely:
- Unsubmitted: Block commitments in queue for submission.
- Submitted: Block commitments that are successfully submitted on L1.
- Confirmed: Block commitments that have become available on-chain. This is
  needed for knowing when to submit the next unsubmitted commitment transaction.
- Merged: Block commitments that are merged into the confirmed state.

Each entry insertion is accompanied by updating relevant `AddressHistoryDB`
entries' statuses.

### Blocks Transactions

### Ledger

We have four ledgers, 3 of which are instances of the `Ledger` abstraction:
- MempoolLedger: The most up-to-date ledger, which is
- LatestLedger
- ConfirmedLedger

The fourth one, the Merkle Patricia Trie (MPT), is a LevelDB instance.

### Transactions

### User Events
