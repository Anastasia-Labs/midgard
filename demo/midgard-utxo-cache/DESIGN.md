# Midgard UTxO Cache Library Design

## Purpose

Midgard should replace flow-local wallet overlays and ad hoc provider refreshes
with a standalone UTxO cache package for transaction-building paths that need
stable input ownership semantics. The immediate production issue is operator
wallet input reuse under concurrent commit/scheduler/merge activity: a
fresh provider read can still return an input that another local path has
already submitted to spend, causing `unknownOutputReferences`, `BadInputsUTxO`,
or related submit failures.

The new package should live in a whole new demo workspace directory, parallel
to `midgard-core`:

```text
demo/midgard-utxo-cache/
  package.json
  tsconfig.json
  vitest.config.ts
  src/
    index.ts
    out-ref.ts
    tx-inspection.ts
    leases.ts
    operator-cache.ts
    script-cache.ts
    cache-state.ts
    registry.ts
    errors.ts
    lucid/
      index.ts
      tx-inspection.ts
      submit-lifecycle.ts
  tests/
```

The package name should be `@al-ft/midgard-utxo-cache`. It should be
provider-agnostic TypeScript, usable by `midgard-node`, `da-committee-node`, and
future services without depending on node databases, fibers, HTTP routers, or
Midgard protocol validators.

Exports should preserve that boundary:

- `@al-ft/midgard-utxo-cache`: core cache state machines, reservations,
  persistence interfaces, errors, event types, and provider-independent Cardano
  UTxO/effect shapes.
- `@al-ft/midgard-utxo-cache/lucid`: Lucid/CML transaction-inspection adapters
  that derive spent inputs and produced outputs from exact signed transaction
  bodies, plus lifecycle helpers that wrap caller-owned submit callbacks without
  embedding a provider or retry policy.

The package may know Cardano UTxO shapes, payment credentials, script
credentials, reservations, and transaction effects. It must not know Midgard
state-queue, scheduler, DA, merge, or contract-selection rules.

## Source Study Plan

Before implementation, study these USDCX files and port the behavior, not the
Haskell-specific shapes:

- `/home/gumbo/iohk/usdcx-backend/usdcx-offchain/src/Cardano/Iris/OffChain/Env/OperatorUtxoCache.hs`
- `/home/gumbo/iohk/usdcx-backend/usdcx-offchain/src/Cardano/Iris/OffChain/Env/NonceListUtxoCache.hs`
- `/home/gumbo/iohk/usdcx-backend/usdcx-cli/src/Cardano/USDCx/Process/TransactionInspection.hs`
- `/home/gumbo/iohk/usdcx-backend/usdcx-cli/src/Cardano/USDCx/Process/MintEvent/Utils.hs`
- `/home/gumbo/iohk/usdcx-backend/usdcx-server/src/Cardano/USDCx/Server/MintPool/Processor.hs`
- `/home/gumbo/iohk/usdcx-backend/usdcx-offchain/src/Cardano/Iris/OffChain/Blockfrost.hs`

Important USDCX behaviors to preserve:

- Operator cache keeps a thread-safe map of owned UTxOs and a pending-spent set.
- Operator cache reloads from chain but subtracts pending-spent inputs before
  exposing spendable inputs.
- Operator cache records successful submissions atomically by removing spent
  inputs, adding produced outputs, and preserving spent inputs until chain
  confirms they are gone.
- Nonce-list cache is a script cache, not an owned-wallet cache. It merges chain
  state with mempool-produced outputs, filters mempool-spent and locally-spent
  inputs, and supports marking inputs missing after stale-input errors.
- Transaction inspection derives produced cache outputs from the signed or
  submitted transaction body instead of waiting for a later provider query.
- Retry logic separates "reload context" from "drop stale spent inputs"; it does
  not blindly resubmit the same stale input set.

## Core Model

The library should expose two cache families with shared primitives:

```ts
type OutRef = `${string}#${number}`;

type CacheUtxo = {
  readonly txHash: string;
  readonly outputIndex: number;
  readonly address: string;
  readonly assets: Readonly<Record<string, bigint>>;
  readonly datum?: string;
  readonly datumHash?: string;
  readonly scriptRef?: unknown;
};

type TxEffects<U extends CacheUtxo = CacheUtxo> = {
  readonly txHash: string;
  readonly spent: readonly OutRef[];
  readonly produced: readonly U[];
};
```

The shared state machine is:

- `known`: outputs currently believed available to the cache.
- `pendingSpent`: inputs locally submitted for spending but not yet observed
  gone from chain.
- `reserved`: inputs checked out by an in-flight builder before submission.
- `missing`: inputs rejected as stale or externally spent and hidden until a
  successful refresh proves otherwise.
- `submitted`: durable submitted or maybe-submitted transaction attempts keyed
  by tx hash. Each attempt records affected reservations, spent inputs,
  produced outputs, evidence state, and validity/expiry metadata.
- `lastRefresh`: provider tip/time metadata for observability and stale-cache
  diagnostics.

Submitted attempts are the durable source for local produced outputs:

```ts
type SubmissionEvidence =
  | "submitting" // recorded before provider submit is called
  | "accepted" // provider returned accepted
  | "maybeAccepted" // provider outcome was ambiguous
  | "observed" // mempool or chain observed the tx
  | "confirmed"
  | "rejected";

type SubmittedTxAttempt<U extends CacheUtxo = CacheUtxo> = {
  readonly txHash: string;
  readonly evidence: SubmissionEvidence;
  readonly effectsByRole: Readonly<Record<string, TxEffects<U>>>;
  readonly reservationIdsByRole: Readonly<Record<string, readonly string[]>>;
  readonly submittedAtMs: number;
  readonly invalidHereafterSlot?: number;
};
```

Produced outputs have an evidence state:

- `acceptedLocalProduced`: the submit call returned accepted or the tx has been
  observed in mempool/chain; these may be selected only when the selection
  policy explicitly sets `allowLocalProducedInputs`.
- `speculativeLocalProduced`: the tx outcome is ambiguous or only pre-submit
  recorded; these are retained for reconciliation and crash safety, but never
  returned from `listSpendable`.

All mutations that change these sets must be serialized per cache instance, and
multi-cache submission-attempt mutations must be serialized through a registry
transaction. JavaScript is single-threaded inside a process, but Midgard uses
concurrent Effect fibers, asynchronous builders, and separate services; the
package should therefore implement an explicit mutex/queue per cache instance
and provide a persistence adapter for process restart safety.

A refresh must either run entirely inside the cache queue or use a
generation/compare-and-merge protocol. A provider snapshot taken before a later
reservation, submission attempt, or missing marker must not overwrite that later
local state. Refresh writes merge against the latest hidden sets and submitted
effects, not against the local state snapshot captured before the async provider
call.

## Operator-Owned UTxO Cache

`OperatorUtxoCache` is for UTxOs at payment credentials controlled exclusively
by the local service. Examples:

- operator-main wallet inputs used by commit and scheduler-refresh paths
- operator-merge wallet inputs used by merge
- DA L1 submitter wallet, if it is a local payment key controlled by the service
- reference-script funding wallet, if it is used as a normal payment wallet

### Authority Assumption

The service owns the payment credential and is the only legitimate spender. That
does not mean a provider read is always fresh; it means the process can know all
local submitted spends and can safely hide them immediately. If another process
is configured with the same key, that is an operational safety violation unless
both processes share the same durable checkout/cache state.

### Public Facade

```ts
type OperatorWalletSubmitContext<U extends CacheUtxo = CacheUtxo> = {
  readonly presetWalletInputs: readonly U[];
};

type OperatorWalletSubmitCallbackResult<R = void> =
  | LucidSignedTx
  | {
      readonly signed: LucidSignedTx;
      readonly result: R;
    };

type OperatorWalletSubmitOptions<U extends CacheUtxo = CacheUtxo> = {
  readonly select?: SelectionPolicy<U>;
  readonly scriptCheckouts?: readonly ScriptInputCheckout[];
  readonly allowLocalProducedInputs?: boolean;
  readonly invalidHereafterSlot?: number;
};

type OperatorWalletOptions<U extends CacheUtxo = CacheUtxo> = {
  readonly role: string;
  readonly purpose: string;
  readonly defaultSelect: SelectionPolicy<U>;
  readonly submit: (input: {
    readonly signed: LucidSignedTx;
    readonly txHash: string;
  }) => Effect.Effect<SubmitAcceptanceEvidence, TxSubmitError>;
  readonly classifySubmitError: SubmitErrorClassifier;
  readonly allowLocalProducedInputs?: boolean;
};

type OperatorWalletSubmitResult<R = void> = {
  readonly txHash: string;
  readonly accepted: SubmitAcceptanceEvidence | SubmitAmbiguityEvidence;
  readonly result: R;
};

type OperatorWalletSubmitError =
  | TxSignError
  | TxSubmitError
  | UncheckedOutOperatorInputSpentError
  | UncheckedOutScriptInputSpentError
  | UnpreparedCacheSubmissionError
  | SpeculativeProducedInputSelectionError
  | StaleCheckoutInputError;

class OperatorWallet<U extends CacheUtxo = CacheUtxo> {
  submitTx<R = void>(
    buildAndSign: (
      context: OperatorWalletSubmitContext<U>,
    ) => Promise<OperatorWalletSubmitCallbackResult<R>>,
  ): Effect.Effect<OperatorWalletSubmitResult<R>, OperatorWalletSubmitError>;
  submitTx<R = void>(
    options: OperatorWalletSubmitOptions<U>,
    buildAndSign: (
      context: OperatorWalletSubmitContext<U>,
    ) => Promise<OperatorWalletSubmitCallbackResult<R>>,
  ): Effect.Effect<OperatorWalletSubmitResult<R>, OperatorWalletSubmitError>;
}
```

`OperatorWallet.submitTx` is the normal developer-facing API for
operator-owned wallet inputs. It selects and checks out local input candidates
internally, passes only those cache-controlled candidates to the callback as
Lucid `presetWalletInputs`, lets Lucid perform normal balancing/coin selection
over that bounded set, inspects the exact signed transaction body, hides spent
operator inputs before submit, submits with facade-configured evidence handling,
records accepted/maybe/rejected outcomes, and releases checked-out-but-unspent
inputs.

The callback builds and signs normally:

```ts
const program = Effect.gen(function* () {
  return yield* operatorWallet.submitTx(async ({ presetWalletInputs }) => {
    const tx = await buildTx();
    return completeAndSignWithLocalEval(lucid, tx, { presetWalletInputs });
  });
});
```

The selection policy and audit purpose should be configured when constructing
the facade for a role, such as operator-main commit, operator-main scheduler
refresh, or operator-merge. Per-call `select` overrides are for unusual flows;
ordinary call sites should not pass a purpose/selection object.

Normal builders should not manually select or `.collectFrom([feeInput])` a
plain operator fee input. Lucid's completion should choose fee/change inputs
from `presetWalletInputs`. Explicit fee-input collection is an advanced escape
hatch only for a transaction whose protocol semantics truly require a specific
wallet input to be present; ordinary fee funding is not such a requirement.

If the transaction also spends inputs selected by a script cache, pass those
scoped script checkouts through `scriptCheckouts`. This is not a selection knob;
it is submission metadata telling the operator-wallet facade that those
script-cache inputs belong to the same signed transaction and must be recorded
in the same durable registry attempt.

### Core State Machine API

```ts
type OperatorUtxoCacheOptions<U extends CacheUtxo> = {
  readonly role: string;
  readonly paymentCredential: string;
  readonly queryOwnedUtxos: () => Promise<readonly U[]>;
  readonly classify?: (utxo: U) => "spendable" | "ignored";
  readonly persistence?: UtxoCachePersistence<U>;
  readonly logger?: UtxoCacheLogger;
};

class OperatorUtxoCache<U extends CacheUtxo = CacheUtxo> {
  refresh(reason: RefreshReason): Promise<CacheSnapshot<U>>;
  snapshot(): Promise<CacheSnapshot<U>>;
  listSpendable(): Promise<readonly U[]>;
  reserveOne(
    policy: SelectionPolicy<U>,
    purpose: string,
  ): Promise<ReservedOneInputLease<U>>;
  reserveMany(
    policy: SelectionPolicy<U>,
    purpose: string,
  ): Promise<ReservedManyInputLease<U>>;
  release(reservationId: string, reason: ReleaseReason): Promise<void>;
  markMissing(outRefs: readonly OutRef[], reason: string): Promise<void>;
}
```

The core cache API is for the facade and advanced multi-cache flows. The
developer-facing operator-wallet API should not expose reservation terminology.
Internally, checkout is mandatory for any operator input that may be spent by a
future transaction. Builders must not receive raw cache snapshots and then
choose inputs outside the cache. Instead:

1. The facade checks out input candidates from the cache.
2. The cache removes those inputs from the available set immediately.
3. If build/sign fails before submit, the facade releases the checkout.
4. Before provider submission, the facade records a durable submission attempt
   through the cache registry. Checked-out spent inputs become hidden
   `submitting` inputs across process restart.
5. If submit is accepted, the registry promotes the attempt to `accepted`; the
   checkout becomes pending-spent and accepted local-produced outputs may be
   selected only if policy allows local-produced inputs.
6. If submit outcome is ambiguous, the registry promotes the attempt to
   `maybeAccepted`; spent inputs stay hidden, but speculative produced outputs
   are not selectable until accepted/observed evidence exists.
7. If submit definitively fails before network acceptance with no possible
   ledger effect, the facade releases the checkout or marks it missing based
   on the failure class.

`recordSubmitted` is intentionally not a per-cache public method. Submission
attempts that affect one or more caches must be recorded through
`UtxoCacheRegistry`, which can atomically update all affected roles.

### Refresh Rules

Operator cache refresh:

- queries all UTxOs for the payment credential;
- subtracts `pendingSpent`, `reserved`, and `missing`;
- adds accepted or observed local-produced outputs from submitted attempts when
  policy allows local-produced inputs;
- keeps speculative local-produced outputs for reconciliation but never exposes
  them as spendable;
- removes a pending-spent input only after the provider no longer reports that
  outref at the credential;
- clears `missing` only when the outref is absent from provider state or when a
  diagnostic policy explicitly allows re-admission after a full refresh.

Refresh writes must merge against the latest reservation, missing, and
submitted-attempt state. An older provider query must not re-admit an input that
was reserved, submitted, or marked missing while that query was in flight.

For owned payment credentials, mempool scanning is optional. The cache already
knows locally submitted spends and locally produced change outputs from the tx
body. Mempool can be used as extra evidence, but the cache must not depend on
provider mempool completeness for safety.

### Produced Outputs

Produced outputs must be derived from the signed/submitted transaction body by
matching outputs that pay to the cache payment credential. This is the same
principle as USDCX `utxosProducedByCredential`. The output index comes from the
transaction body, not a later provider fetch.

This lets a later local transaction spend newly produced change before the
provider has indexed it, but only when the same cache registry has accepted or
observed evidence for the producer transaction and the submit path treats that
dependency as local chain state. Consumers that cannot tolerate tx chaining
should keep `allowLocalProducedInputs: false`, which should be the default for
operator wallet-input selection until a call site explicitly proves it wants
local tx chaining.

## Script UTxO Cache

`ScriptUtxoCache` generalizes USDCX `NonceListUtxoCache`. It is for UTxOs at a
script credential where the local service does not have perfect spend authority.
Examples:

- Midgard state-queue UTxOs
- scheduler or active-operator script UTxOs
- DA attestation candidate UTxOs
- nonce-list style linked-list UTxOs in other protocols

### Authority Assumption

The local service may produce or spend script outputs, but other valid
transactions can also spend them. The cache is therefore a reconciliation aid,
not an authoritative source of truth. It must provide fast local consistency
without pretending external spends cannot happen.

### Public Facade

```ts
type ScriptSelectionContext<U extends CacheUtxo = CacheUtxo> = {
  readonly candidates: readonly U[];
  readonly snapshot: CacheSnapshot<U>;
};

type Opaque<TypeName extends string> = unknown & {
  readonly __opaque: TypeName;
};

type ScriptCheckoutToken = Opaque<"ScriptCheckoutToken">;

type ScriptInputCheckoutBase<U extends CacheUtxo = CacheUtxo> = {
  readonly kind: "script-input-checkout";
  readonly role: string;
  readonly label?: string;
  readonly token: ScriptCheckoutToken;
  readonly outRefs: readonly OutRef[];
};

type ScriptOneInputCheckout<U extends CacheUtxo = CacheUtxo> =
  ScriptInputCheckoutBase<U> & {
    readonly mode: "one";
    readonly utxo: U;
  };

type ScriptManyInputCheckout<U extends CacheUtxo = CacheUtxo> =
  ScriptInputCheckoutBase<U> & {
    readonly mode: "many";
    readonly utxos: readonly U[];
  };

type ScriptInputCheckout<U extends CacheUtxo = CacheUtxo> =
  | ScriptOneInputCheckout<U>
  | ScriptManyInputCheckout<U>;

type ScriptSelectOneOptions<U extends CacheUtxo = CacheUtxo> = {
  readonly label?: string;
  readonly refresh?: "if-empty" | "always" | "never";
  readonly select: (context: ScriptSelectionContext<U>) => U | OutRef;
};

type ScriptSelectManyOptions<U extends CacheUtxo = CacheUtxo> = {
  readonly label?: string;
  readonly refresh?: "if-empty" | "always" | "never";
  readonly select: (
    context: ScriptSelectionContext<U>,
  ) => readonly U[] | readonly OutRef[];
};

type ScriptUtxoCacheOptions<U extends CacheUtxo> = {
  readonly role: string;
  readonly scriptCredential: string;
  readonly queryChainUtxos: () => Promise<readonly U[]>;
  readonly queryMempoolEffects?: () => Promise<MempoolEffects<U>>;
  readonly matches: (utxo: U) => boolean;
  readonly decode?: (utxo: CacheUtxo) => U | undefined;
  readonly persistence?: UtxoCachePersistence<U>;
  readonly logger?: UtxoCacheLogger;
};

class ScriptUtxoCache<U extends CacheUtxo = CacheUtxo> {
  refresh(reason: RefreshReason): Promise<CacheSnapshot<U>>;
  snapshot(): Promise<CacheSnapshot<U>>;
  listSpendable(): Promise<readonly U[]>;
  withSelectedOne<R, E = never>(
    options: ScriptSelectOneOptions<U>,
    use: (checkout: ScriptOneInputCheckout<U>) => Effect.Effect<R, E>,
  ): Effect.Effect<R, ScriptUtxoCacheError | E>;
  withSelectedMany<R, E = never>(
    options: ScriptSelectManyOptions<U>,
    use: (checkout: ScriptManyInputCheckout<U>) => Effect.Effect<R, E>,
  ): Effect.Effect<R, ScriptUtxoCacheError | E>;
}
```

Script cache selection is usually domain-specific. The generic package should
not embed "choose the state-queue tail" or "choose the oldest DA candidate"
logic. It should expose safe snapshots, optional diagnostic labels, and scoped
selection helpers for adapters that need them. Normal Midgard state-queue code
should use a domain facade such as `queryLatestStateCommitment`, not a
caller-supplied generic selector.

`ScriptUtxoCache` intentionally does not submit transactions. A script cache
does not own a signing key and cannot know the complete protocol transition.
Generic adapters may use `withSelectedOne` or `withSelectedMany` to scope
selected script inputs, then pass those checkouts to the submitter facade that
owns submission, usually `operatorWallet.submitTx({ scriptCheckouts: [...] },
...)`. Midgard state-queue callers should use `queryLatestStateCommitment`
instead. In both cases, the registry gets one atomic record covering both
operator wallet inputs and script inputs spent by the same signed transaction.

The checkout token is opaque. Domain code may read `utxo`, `utxos`, and
`outRefs` for building, logging, and validation, but it should not manage
reservation ids. `withSelectedOne` and `withSelectedMany` own the finalizer: if
the callback fails before the checkout is adopted by
`registry.prepareSubmissionAttempt`, the checkout is released; once the
operator-wallet facade adopts it into a durable submission attempt, the
script-cache finalizer no-ops because the registry now owns the accepted,
maybe-accepted, or rejected transition.

`withSelectedOne` and `withSelectedMany` are generic utilities. They are not the
preferred Midgard state-queue API. If a script surface has one obvious protocol
query, expose that query directly instead of asking every caller to provide a
label and selector.

#### Generic Selection Helpers

`withSelectedOne` and `withSelectedMany` are for script-cache surfaces where the
cache can provide a safe candidate set, but the cache package does not know the
domain rule for choosing one or more candidates.

They work like this:

1. read the current spendable cache view, refreshing first when requested;
2. pass the candidate UTxOs to the caller's selector;
3. atomically check out the selected outref or outrefs if they are still
   spendable;
4. invoke the callback with an opaque script checkout;
5. release the checkout if the callback fails before a submitter facade adopts
   it into `registry.prepareSubmissionAttempt`;
6. no-op the finalizer once the submitter facade adopts the checkout, because
   the registry now owns accepted/maybe-accepted/rejected handling.

Use them for generic or uncommon script surfaces that do not deserve their own
domain API yet, for example:

- selecting a DA attestation candidate by datum field;
- selecting a nonce-list node by nonce range in a reusable non-Midgard library;
- tests, diagnostics, or maintenance flows that need scoped script input
  selection without introducing a permanent domain facade.

Do not use them for Midgard's normal state-queue flow. The state-queue has a
single protocol query, "latest state commitment", so normal callers should use
`queryLatestStateCommitment` or `submitWithLatestStateCommitment`.

### Midgard State-Queue Facade

The state-queue cache should expose a domain-specific facade over the generic
script cache. Its normal query is "latest state commitment", implemented by
deriving the unique live tail from the cached state-queue topology. Callers
should not pass a selector.

```ts
type LatestStateCommitment = ScriptOneInputCheckout<SDK.StateQueueUTxO>;

type QueryLatestStateCommitmentOptions = {
  readonly refresh?: "if-empty" | "always" | "never";
  readonly label?: string;
};

class StateQueueCommitmentCache {
  queryLatestStateCommitment(
    options?: QueryLatestStateCommitmentOptions,
  ): Effect.Effect<LatestStateCommitment, StateQueueCommitmentCacheError>;

  markStateCommitmentMissing(
    latest: LatestStateCommitment,
    reason: string,
  ): Effect.Effect<void, StateQueueCommitmentCacheError>;

  submitWithLatestStateCommitment<R, E>(
    submit: (latest: LatestStateCommitment) => Effect.Effect<R, E>,
  ): Effect.Effect<R, E | StateQueueCommitmentCacheError>;
}
```

`queryLatestStateCommitment` should read from cache when the cache already has a
healthy state-queue topology. If the cache is empty, unhealthy, or explicitly
forced with `refresh: "always"`, it refreshes from chain/mempool, decodes the
state-queue nodes, requires exactly one root and one tail, and returns the tail
as the latest commitment checkout. The returned value should not duplicate
fields already carried by `SDK.StateQueueUTxO`: the outref is derived from
`utxo.utxo.txHash` plus `utxo.utxo.outputIndex`, the header hash is derived by
`SDK.headerHashFromStateQueueUTxO`, and roots/end time are decoded from the
state-queue datum when a caller needs them. If submission later proves that
outref stale, the caller or `submitWithLatestStateCommitment` marks that
specific commitment missing, removes it from the spendable cache view,
refreshes, and rebuilds against the new latest commitment.

`submitWithLatestStateCommitment` should be the production convenience wrapper:
query latest from cache, call the supplied submit callback, and if the submit
error references that state-commitment outref as stale, mark only that outref
missing, force-refresh, query latest again, and retry once. It must not silently
loop forever or swallow non-stale submit errors.

This mirrors the useful part of USDCX `NonceListUtxoCache`: cached script UTxOs
are a fast local view, successful transactions update spent/produced overlays,
and stale/missing script inputs are removed from the cache before retrying.
Midgard should improve the shape by wrapping that behavior in a state-queue
domain API, not by requiring every caller to provide a generic selection
function.

### Core State Machine API

```ts
class ScriptUtxoCache<U extends CacheUtxo = CacheUtxo> {
  reserveOne(outRef: OutRef, label?: string): Promise<ReservedOneInputLease<U>>;
  reserveMany(
    outRefs: readonly OutRef[],
    label?: string,
  ): Promise<ReservedManyInputLease<U>>;
  release(reservationId: string, reason: ReleaseReason): Promise<void>;
  markMissing(outRefs: readonly OutRef[], reason: string): Promise<void>;
}
```

The primitive reservation methods remain for adapters, tests, diagnostics, and
rare multi-step flows. They should not be the examples copied into normal
Midgard transaction paths.

### Refresh Rules

Script cache refresh:

- queries chain UTxOs at the script credential and filters/decodes only matching
  UTxOs;
- if a mempool effect provider exists, subtracts mempool-spent script inputs and
  adds mempool-produced matching script outputs;
- subtracts locally `pendingSpent`, `reserved`, and `missing` outrefs;
- adds accepted/observed locally produced matching script outputs from submitted
  attempts;
- retains speculative local-produced outputs for reconciliation but does not
  expose them as spendable;
- preserves missing markers until a refresh proves the outref absent or a domain
  recovery path explicitly re-admits it.

Unlike the operator cache, script cache must assume any cached input can become
invalid externally. Stale-script-input submit failures should call
`markMissing`, refresh script context, and rebuild from the new snapshot.

A refresh write must merge against the latest local hidden sets and submitted
attempts. A provider or mempool snapshot captured before a later local
reservation, submission attempt, or missing marker must not overwrite that later
state.

Mempool absence is never freshness proof. `queryMempoolEffects` may strengthen a
snapshot by subtracting observed spends and adding observed produced outputs,
but incomplete or failing mempool scans must not re-admit locally hidden inputs
or clear missing/pending state. Cache events should include mempool source
metadata and completeness/failure status.

## Internal Checkout Semantics

Internal checkout prevents two local builders from selecting the same input. It
is not the same as submitted/pending-spent:

- `reserved`: selected by a local builder, not known submitted.
- `pendingSpent`: submitted in a tx whose ledger outcome is not yet confirmed or
  whose spend has not yet disappeared from chain/provider view.
- `missing`: known bad from a submit/build error or external invalidation.

Checkout records should include:

- reservation id
- cache role
- outref
- optional diagnostic label, such as `commit-fee`, `scheduler-refresh-fee`, or
  `state-queue-latest`
- created timestamp
- optional deadline
- optional tx hash after submission

Expired checkouts should not silently become spendable. The default should be
strict: stale checkouts require an explicit release or a recovery command.
For e2e/dev convenience, a caller may use a named recovery method that logs the
forced release with reason and age, but production paths should prefer fail-fast
readiness errors.

## Cache Registry

`UtxoCacheRegistry` owns cache instances and all multi-cache submission attempt
transitions. It is the only production path for recording transaction effects.

```ts
type SubmissionAttempt<U extends CacheUtxo = CacheUtxo> = {
  readonly txHash: string;
  readonly effectsByRole: Readonly<Record<string, TxEffects<U>>>;
  readonly reservationIdsByRole: Readonly<Record<string, readonly string[]>>;
  readonly invalidHereafterSlot?: number;
};

type ReservationRequestBase<U extends CacheUtxo = CacheUtxo> = {
  readonly role: string;
  readonly label?: string;
};

type OneReservationRequest<U extends CacheUtxo = CacheUtxo> =
  ReservationRequestBase<U> & {
    readonly mode?: "one";
    readonly select?: SelectionPolicy<U>;
    readonly reserve?: (context: {
      readonly candidates: readonly U[];
    }) => U | OutRef;
  };

type ManyReservationRequest<U extends CacheUtxo = CacheUtxo> =
  ReservationRequestBase<U> & {
    readonly mode: "many";
    readonly select?: SelectionPolicy<U>;
    readonly reserve?: (context: {
      readonly candidates: readonly U[];
    }) => readonly U[] | readonly OutRef[];
  };

type ReservationRequest<U extends CacheUtxo = CacheUtxo> =
  | OneReservationRequest<U>
  | ManyReservationRequest<U>;

type NamedOneReservationRequest<U extends CacheUtxo = CacheUtxo> =
  OneReservationRequest<U> & {
    readonly as: string;
  };

type NamedManyReservationRequest<U extends CacheUtxo = CacheUtxo> =
  ManyReservationRequest<U> & {
    readonly as: string;
  };

type NamedReservationRequest<U extends CacheUtxo = CacheUtxo> =
  | NamedOneReservationRequest<U>
  | NamedManyReservationRequest<U>;

type LeaseForRequest<Request> =
  Request extends NamedManyReservationRequest<infer U>
    ? ReservedManyInputLease<U>
    : Request extends NamedOneReservationRequest<infer U>
      ? ReservedOneInputLease<U>
      : never;

type NamedReservationLeases<
  Requests extends readonly NamedReservationRequest[],
> = {
  readonly [Request in Requests[number] as Request["as"]]: LeaseForRequest<Request>;
};

class UtxoCacheRegistry<U extends CacheUtxo = CacheUtxo> {
  operator(role: string): OperatorUtxoCache<U>;
  script(role: string): ScriptUtxoCache<U>;
  prepareSubmissionAttempt(attempt: SubmissionAttempt<U>): Promise<void>;
  recordAccepted(
    txHash: string,
    evidence: SubmitAcceptanceEvidence,
  ): Promise<void>;
  recordMaybeAccepted(
    txHash: string,
    evidence: SubmitAmbiguityEvidence,
  ): Promise<void>;
  recordRejected(txHash: string, decision: RejectionDecision): Promise<void>;
  recordObserved(txHash: string, evidence: ObservationEvidence): Promise<void>;
  recordConfirmed(
    txHash: string,
    evidence: ConfirmationEvidence,
  ): Promise<void>;
  withReservation<A>(
    request: ReservationRequest<U>,
    use: (
      lease: ReservedOneInputLease<U> | ReservedManyInputLease<U>,
    ) => Promise<A>,
  ): Promise<A>;
  withReservations<
    A,
    const Requests extends readonly NamedReservationRequest<U>[],
  >(
    requests: Requests,
    use: (leases: NamedReservationLeases<Requests>) => Promise<A>,
  ): Promise<A>;
}
```

Registry invariants:

- `prepareSubmissionAttempt` is durable and happens before the provider submit
  call. It moves affected reserved inputs into hidden `submitting` state.
- `effectsByRole` must be derived from the exact signed transaction body.
- `reservationIdsByRole` must reference reservations already held by the caller.
- Script-cache spent inputs must be backed by supplied script checkout tokens;
  otherwise preparation fails before provider submission.
- A transaction that affects multiple roles is persisted as one atomic event or
  not at all.
- Accepted and observed attempts may expose produced outputs only under an
  explicit selection policy.
- Maybe-accepted attempts keep spent inputs hidden but keep produced outputs
  speculative and non-spendable.
- Rejected attempts release unspent reservations or mark stale inputs missing
  according to the rejection decision.

## Developer Experience

The safe state machine should not force every call site to hand-roll the same
reservation, inspection, submit, promotion, release, and stale-input logic. The
library should provide role-specific facades that make the safe path the easy
path and keep checkout/reservation terms out of normal application code.

### DevEx Review Findings

An independent DevEx review of this design returned `REQUEST_DEVEX_CHANGES`.
The safety model was sound, but the first ergonomic layer still made normal
Midgard call sites understand too much cache lifecycle state. The review
compared the design against the existing commit, scheduler-refresh, merge, and
submit helper shapes and found these problems:

- `reserveOne` plus `prepareSubmissionAttempt` is easy to misuse because the
  caller must manually keep reservation ids, tx effects, and release behavior in
  sync.
- `presetWalletInputs` was too easy to confuse with raw Lucid wallet state if a
  developer reserves one input but passes the whole provider wallet view to
  Lucid completion.
- A direct registry API encourages repetitive `try/catch` blocks at every
  submitter and makes it too easy to forget `TxSignError` release or
  ambiguous-submit produced-output rules.
- Script-cache use is clean for domain code only if the cache returns typed
  candidate snapshots and opaque scoped checkouts, not raw internal state.
- Scheduler refresh and merge need an Effect-native replacement for the current
  wallet-view plus `handleSignSubmit` shape; otherwise each integration would
  invent its own partial lifecycle wrapper.

The implementation should therefore expose `OperatorWallet.submitTx` as the
primary path for operator-owned wallet inputs. Reservation/lease handles remain
internal implementation details and advanced escape hatches; normal Midgard code
should not name them.

```ts
const program = Effect.gen(function* () {
  return yield* operatorWallet.submitTx(async ({ presetWalletInputs }) => {
    const tx = await buildTx();
    return completeAndSignWithLocalEval(lucid, tx, { presetWalletInputs });
  });
});
```

The common path should read as:

1. call the role-specific operator-wallet facade;
2. build normally without manually selecting a fee input;
3. let the facade inspect the signed body, prepare/hide spent inputs, submit,
   and record accepted/maybe/rejected outcomes.

`listSpendable()` is for candidate selection only. Callers must not pass
`listSpendable()` results directly to Lucid builders. For operator wallets,
normal callers should not call `listSpendable()` at all; the facade owns
selection and checkout.

Raw provider wallet reads must not appear in default cache-aware examples. The
Lucid adapter may expose `unsafe.uncachedPresetWalletInputs(...)` only as an
explicit diagnostic escape hatch; production examples should never use it.

The examples below use the Midgard Effect service facade, where
`operatorWallet.submitTx` returns an `Effect` value. The core registry can still
expose Promise-returning methods for non-Effect consumers, but Midgard node
integrations should not hand-convert every cache operation at each call site.

### Operator Wallet Submit Helper

The role-specific operator wallet should provide a helper that matches
Midgard's Effect style without taking ownership of retry policy or protocol
rules:

```ts
const submitSignedTxWithAcceptanceEvidence = (
  lucid: Lucid,
  signed: LucidSignedTx,
  txHash: string,
  options: SubmitRecoveryOptions,
) =>
  submitSignedTxWithRecovery(lucid, signed, txHash, options).pipe(
    Effect.as({
      kind: "provider-accepted",
      txHash,
    } satisfies SubmitAcceptanceEvidence),
  );

const program = Effect.gen(function* () {
  const result = yield* operatorWallet.submitTx(
    async ({ presetWalletInputs }) => {
      const tx = await buildTx();
      return completeAndSignWithLocalEval(lucid, tx, { presetWalletInputs });
    },
  );

  return result;
});
```

Here `operatorWallet` was constructed with the role, purpose, default selection
policy, submit evidence callback, and submit-error classifier:

```ts
const operatorWallet = createOperatorWallet({
  role: "operator-main",
  purpose: "commit-fee",
  defaultSelect: selectPureAdaInputs({ maxInputs: 3, minTotalLovelace }),
  submit: ({ signed, txHash }) =>
    submitSignedTxWithAcceptanceEvidence(
      lucid,
      signed,
      txHash,
      submitRecoveryOptions,
    ),
  classifySubmitError: classifyMidgardCacheSubmitError({
    staleRoles: ["operator-main"],
  }),
});
```

`operatorWallet.submitTx` must pass only checked-out operator inputs as
`presetWalletInputs`. It must inspect the exact signed transaction body returned
by the callback, reject any transaction that spends an operator input it did not
check out, prepare the durable registry attempt before invoking `submit`, hide
spent inputs across restart, and release checked-out-but-unspent inputs after an
accepted or definitively rejected outcome. `submit` must resolve with
`SubmitAcceptanceEvidence` only when the provider accepted the transaction;
ambiguous and definitive failures should be thrown so `classifySubmitError` can
decide whether to record `maybeAccepted`, mark stale inputs missing, or reject
and release. The helper does not retry, rebuild, wait for confirmation, or hide
failures from the caller.

### Example: Commit Wallet Inputs

For commit paths that only need operator wallet funding, the call site should
stay this small:

```ts
const commitProgram = Effect.gen(function* () {
  return yield* operatorMainWallet.submitTx(async ({ presetWalletInputs }) => {
    const built = await buildProductionCommitBlockHeaderTxProgram({
      ...commitArgs,
      latestBlock,
    });

    return completeAndSignWithLocalEval(lucid, built.tx, {
      presetWalletInputs,
    });
  });
});
```

The facade is configured at construction time with the role `operator-main`,
default input selection, role extractors, submit evidence callback, and
submit-error classifier. The call site does not pass a purpose string or
selection policy unless it is intentionally overriding the default.

### Example: Scheduler Refresh

Scheduler refresh may need a small set of operator inputs for Lucid completion,
but that policy should be configured on the scheduler-refresh wallet facade, not
repeated at every call site.

```ts
const refreshProgram = Effect.gen(function* () {
  const refreshResult = yield* operatorSchedulerWallet.submitTx(
    async ({ presetWalletInputs }) => {
      const built = await buildUnsignedSchedulerRefreshTxProgram({
        ...refreshArgs,
      });

      const signed = await completeAndSignWithLocalEval(lucid, built.tx, {
        presetWalletInputs,
      });
      return {
        signed,
        result: {
          refreshedWitness: built.refreshedWitness,
          schedulerSelection: built.selection,
        },
      };
    },
  );

  return refreshResult;
});
```

When the callback returns extra `result` data, `submitTx` should preserve it
beside the submitted tx hash and acceptance evidence. This keeps scheduler code
from reimplementing wallet overlay mutation just to keep its domain-specific
return values.

### Example: Latest State Commitment

Midgard state-queue callers should not spell out a generic script-cache
selector. The state-queue surface has one obvious query: resolve the current
latest state commitment, using cached state if available and refreshing from
chain/mempool when the cache is empty or stale.

```ts
const submitCommitWithLatestStateCommitment = Effect.gen(function* () {
  const submitUsing = (latest: LatestStateCommitment) =>
    operatorMainWallet.submitTx(
      { scriptCheckouts: [latest] },
      async ({ presetWalletInputs }) => {
        const built = await buildCommitUsingTail({
          ...args,
          latestBlock: latest.utxo,
        });

        return completeAndSignWithLocalEval(lucid, built.tx, {
          presetWalletInputs,
        });
      },
    );

  const latest = yield* stateQueueCommitments.queryLatestStateCommitment();

  return yield* submitUsing(latest).pipe(
    Effect.catchIf(
      (error) => isStaleScriptInputError(error, stateQueueOutRef(latest.utxo)),
      () =>
        Effect.gen(function* () {
          yield* stateQueueCommitments.markStateCommitmentMissing(
            latest,
            "commit-tail-stale",
          );
          const refreshed =
            yield* stateQueueCommitments.queryLatestStateCommitment({
              refresh: "always",
            });
          return yield* submitUsing(refreshed);
        }),
    ),
  );
});
```

The production implementation should hide the duplicated retry shape behind a
small domain helper, for example
`stateQueueCommitments.submitWithLatestStateCommitment`, but the important API
boundary is the same: callers do not pass `purpose`, do not provide
`selectCanonicalLiveTail`, and do not inspect raw cache state. They query the
latest state commitment, build against it, and stale submit feedback evicts that
specific outref before retrying against a refreshed latest commitment.

`scriptCheckouts` are already scoped inputs, not per-call operator selection
policy. Passing them to `operatorMainWallet.submitTx` ensures the signed body is
checked against the selected script input and the registry records operator and
state-queue effects atomically.

### Example: Merge Wallet

Merge should look nearly identical to commit, except the role is
`operator-merge`. The distinct facade makes accidental
operator-main/operator-merge reuse visibly wrong in code review.

```ts
const mergeProgram = Effect.gen(function* () {
  return yield* operatorMergeWallet.submitTx(async ({ presetWalletInputs }) => {
    const built = await buildProductionMergeToConfirmedStateTxProgram({
      ...mergeArgs,
    });

    return completeAndSignWithLocalEval(lucid, built.tx, {
      presetWalletInputs,
    });
  });
});
```

The merge code remains responsible for maturity and validity-window rules. The
operator-wallet facade only manages operator-owned wallet inputs, signed-body
effects, and submit evidence.

### Internal Lifecycle

The operator-wallet facade may be implemented on top of lower-level handles like
these, but normal Midgard call sites should not use them directly:

```ts
type ReservedOneInputLease<U extends CacheUtxo = CacheUtxo> = {
  readonly role: string;
  readonly label?: string;
  readonly reservationIds: readonly string[];
  readonly utxo: U;
  readonly utxos: readonly [U];
  readonly outRef: OutRef;
  readonly outRefs: readonly OutRef[];
  toLucidPresetWalletInputs(): readonly U[];
  release(reason: ReleaseReason): Promise<void>;
};

type ReservedManyInputLease<U extends CacheUtxo = CacheUtxo> = {
  readonly role: string;
  readonly label?: string;
  readonly reservationIds: readonly string[];
  readonly utxos: readonly U[];
  readonly outRefs: readonly OutRef[];
  toLucidPresetWalletInputs(): readonly U[];
  release(reason: ReleaseReason): Promise<void>;
};

type PreparedSubmission<U extends CacheUtxo = CacheUtxo> = {
  readonly txHash: string;
  readonly spentByRole: Readonly<Record<string, readonly OutRef[]>>;
  readonly producedByRole: Readonly<Record<string, readonly U[]>>;
  readonly signed: unknown;
  accepted(evidence: SubmitAcceptanceEvidence): Promise<void>;
  maybeAccepted(evidence: SubmitAmbiguityEvidence): Promise<void>;
  rejected(decision: RejectionDecision): Promise<void>;
};
```

The internal lifecycle is:

1. select and check out operator inputs from the cache;
2. attach any already scoped script checkouts passed in `scriptCheckouts`;
3. pass checked-out inputs to the callback as `presetWalletInputs`;
4. complete/sign through the callback;
5. inspect the exact signed body;
6. fail if the signed body spends un-checked-out operator inputs or
   cache-controlled script inputs outside the supplied script checkouts;
7. prepare one durable submission attempt and hide all spent checked-out inputs
   before submit;
8. submit through the configured evidence callback;
9. mark accepted/maybe/rejected internally for every affected cache role;
10. expose produced operator or script outputs only when accepted/observed
    evidence and role policy allow;
11. release checked-out-but-unspent inputs.

### Advanced Escape Hatch: Manual Lifecycle

The primitive lifecycle remains available for unusual flows, tests, and
diagnostics, but it should not be the shape copied into normal Midgard
submitters:

```ts
const walletInputs = await registry
  .operator("operator-main")
  .reserveMany(selectCompletionWalletInputs({ maxInputs: 4 }), "commit");

let signed!: LucidSignedTx;
let prepared!: PreparedSubmission;

try {
  const built = await buildProductionCommitBlockHeaderTxProgram({
    ...commitArgs,
  });
  signed = await completeAndSignWithLocalEval(lucid, built.tx, {
    presetWalletInputs: walletInputs.toLucidPresetWalletInputs(),
  });
  prepared = await lucidSubmission.prepareFromSignedTx({
    signed,
    leases: [walletInputs],
    roleExtractors: midgardCommitRoleExtractors(contracts, operatorMainAddress),
    invalidHereafterSlot: built.invalidHereafterSlot,
  });
} catch (error) {
  await walletInputs.release({ reason: "commit-build-or-sign-failed", error });
  throw error;
}

try {
  await submitSignedTx(lucid, signed);
  await prepared.accepted({
    kind: "provider-accepted",
    txHash: prepared.txHash,
  });
} catch (error) {
  await prepared.rejected(
    classifyMidgardCacheSubmitError({
      staleRoles: ["operator-main"],
    })(error),
  );
  throw error;
}
```

Manual code has to prove it releases leases exactly once and records every
prepared attempt outcome. The operator-wallet facade is the recommended
production surface.

### DevEx Requirements

- Normal operator-owned wallet call sites should use
  `operatorWallet.submitTx(...)`; they should not call `reserveOne`,
  `reserveMany`, `withReservations`, or `signPrepareSubmitEffect` directly.
- Normal state-queue call sites should use `queryLatestStateCommitment` or a
  small domain wrapper built on it. They should not pass `purpose`, write a
  selector, or inspect generic script-cache snapshots.
- Generic script-cache helpers may expose `withSelectedOne(...)` or
  `withSelectedMany(...)` for script surfaces without a domain facade, but their
  labels must be optional diagnostics.
- The default examples and exported helpers must pass only checked-out operator
  UTxOs as `presetWalletInputs`.
- The `submitTx` callback should receive only domain-useful values:
  `presetWalletInputs` and optional facade-specific helpers. It should not
  receive wallet-input leases, reservation ids, registry attempts, or cache
  state.
- Normal call sites should let Lucid balance from `presetWalletInputs`; they
  should not manually select or collect a wallet fee input.
- The facade should be configured once per role/purpose with default selection,
  role extractors, submit evidence callback, and submit-error classifier.
- Per-call selection overrides should exist for unusual flows, but the shortest
  valid example must be callback-only `operatorWallet.submitTx(...)`.
- Script caches should not expose a normal `submitTx` facade. They do not own a
  signing key or transaction submission boundary; they scope selected script
  inputs and rely on the submitter facade to record the final signed-body
  effects.
- The API should make the wrong code look strange. Passing a raw
  `lucid.wallet().getUtxos()` result directly into a cache-aware builder should
  require an explicit unsafe namespace such as
  `lucidSubmission.unsafe.uncachedPresetWalletInputs`.
- Error names should point to the fix: `UnpreparedCacheSubmissionError`,
  `UncheckedOutOperatorInputSpentError`,
  `UncheckedOutScriptInputSpentError`, `SpeculativeProducedInputSelectionError`,
  `StaleCheckoutInputError`.

### Rejected Normal API Shape

Do not make ordinary callers write this:

```ts
const program = Effect.gen(function* () {
  return yield* operatorWallet.submitTx(
    {
      purpose: "commit-fee",
      select: selectPureAdaInputs({ maxInputs: 3, minTotalLovelace }),
    },
    async ({ presetWalletInputs }) => {
      const tx = await buildTx();
      return completeAndSignWithLocalEval(lucid, tx, { presetWalletInputs });
    },
  );
});
```

`purpose` is useful for audit logs, and selection policy is necessary, but those
belong in facade construction for common Midgard paths. Requiring that object at
every call site reintroduces cache machinery into application code.

## Submission Lifecycle

All Midgard transaction submitters that use operator-cache-selected inputs
should normally enter through `operatorWallet.submitTx`. Internally that facade
follows this lifecycle:

1. `refresh` at service startup and after known stale-input failures.
2. Check out one or more operator wallet input candidates immediately before
   invoking the caller's build/sign callback.
3. If the transaction spends the state-queue tail, resolve it through
   `queryLatestStateCommitment` and pass the returned checkout to the
   operator-wallet submit call.
4. Build the transaction without manually selecting a wallet fee input, using
   protocol-selected script inputs where applicable.
5. Complete and sign the transaction in the caller callback with the
   cache-controlled `presetWalletInputs` supplied by the facade.
6. Inspect the signed transaction body to compute tx hash, spent inputs,
   produced outputs, and affected cache roles.
7. Fail before submit if the signed body spends an operator-owned input that the
   facade did not check out, or if it spends a cache-controlled script input
   that was not included in `scriptCheckouts`.
8. Before calling the provider submit API, call
   `registry.prepareSubmissionAttempt`. This durably moves spent checked-out
   operator and script inputs into hidden `submitting` state across process
   restart. Produced outputs are recorded but not spendable yet.
9. Submit the transaction.
10. If submission returns accepted, call `registry.recordAccepted`. Accepted
    produced outputs may be selected only when `allowLocalProducedInputs` is true.
11. If submission outcome is ambiguous, call `registry.recordMaybeAccepted`.
    Spent inputs remain hidden, but produced outputs stay speculative and are
    not returned from `listSpendable` until tx hash reconciliation provides
    accepted, observed, or confirmed evidence.
12. If submission definitively failed before ledger acceptance, call
    `registry.recordRejected`. The rejection decision releases ordinary
    checkouts or marks stale selected operator/script inputs missing.
13. On startup, persisted `submitting` and `maybeAccepted` attempts keep their
    spent inputs hidden until tx-hash status, validity expiry, or explicit
    operator recovery proves the transaction cannot land.
14. Confirmation watchers call `refresh` after local finalization or observed
    confirmation, mainly to prune pending-spent and reconcile external script
    changes.

Any wallet input made visible to Lucid completion through `presetWalletInputs`
must be cache-controlled. The operator-wallet facade should expose only
checked-out inputs to Lucid completion. If Lucid spends only a subset of the
checked-out candidates, the facade transitions actually spent inputs to
submitting/pending state and releases checked-out-but-unspent inputs.

Cache-aware Midgard submitters must use a submit path that preserves signed
transaction context inside the facade, not only a tx hash.
`handleSignSubmitNoConfirmation` currently drops the signed CBOR/context;
operator-wallet call sites should use `operatorWallet.submitTx` or
context-returning wrappers so the registry can record effects from the exact
signed body. `TxSignError` releases checkouts. Definitive non-acceptance
releases or marks missing. `unknownOutputReferences` for a selected operator
wallet input marks that input missing and rebuilds; the same error for a
selected script input marks that script input missing, refreshes the script
context, and rebuilds. Neither case is commit-success or merge-success
recovery.

The package should include helper functions for common Lucid transaction
inspection:

- extract all input outrefs from a transaction body;
- extract produced outputs matching a payment credential;
- extract produced outputs matching a script credential plus predicate;
- classify `unknownOutputReferences`, `BadInputsUTxO`,
  `TranslationLogicMissingInput`, and `ValueNotConservedUTxO` as stale-input
  candidates.

The package should not include provider-specific transaction submitters and
should not own retry loops. The operator-wallet facade may wrap a caller-owned
submit callback only to keep cache transitions correct. The package should
return typed errors and structured cache events that the caller can use to
decide whether to rebuild, retry, or fail.

## Persistence

The first implementation may use in-memory state for unit tests, but the design
must support durable persistence because Midgard can crash after submitting a
transaction and before confirmation.

```ts
type UtxoCachePersistence<U extends CacheUtxo> = {
  load(role: string): Promise<PersistedCacheState<U> | undefined>;
  save(role: string, state: PersistedCacheState<U>): Promise<void>;
  appendEvent?(role: string, event: UtxoCacheEvent<U>): Promise<void>;
};

type UtxoCacheTransactionStore<U extends CacheUtxo> = {
  loadMany(
    roles: readonly string[],
  ): Promise<ReadonlyMap<string, PersistedCacheState<U>>>;
  commit(events: readonly UtxoCacheEvent<U>[]): Promise<void>;
};
```

For `midgard-node`, the production adapter should be database-backed, not a JSON
sidecar. The cache role should be part of the key so operator-main, merge,
reference, DA submitter, and script caches cannot contaminate each other.
Transactions that affect multiple cache roles must write all cache events in one
database transaction through `UtxoCacheTransactionStore.commit`. A crash after
recording operator effects but before script effects, or vice versa, must not be
possible in the production adapter.

Startup behavior:

- load persisted state;
- refresh against provider before serving;
- keep pending-spent/reserved/missing hidden during refresh;
- keep `submitting` and `maybeAccepted` attempts hidden while tx status is
  reconciled;
- refuse readiness if there are old unresolved reservations or submitting
  attempts that cannot be explained by a pending submitted tx, validity window,
  or explicit recovery record.

## Midgard Integration Plan

### Phase 1: Package and Tests

- Add `demo/midgard-utxo-cache` to `demo/pnpm-workspace.yaml`.
- Implement core types, outref helpers, in-memory persistence, mutexed state
  transitions, operator cache, script cache, `UtxoCacheRegistry`, and
  tx-inspection helpers.
- Implement Lucid/CML adapters under `@al-ft/midgard-utxo-cache/lucid`, not in
  the core export.
- Add tests for:
  - concurrent checkout of the same input;
  - refresh preserving pending-spent inputs;
  - produced wallet outputs becoming available only after accepted/observed
    registry evidence;
  - speculative produced outputs never becoming spendable;
  - pre-submit attempts hiding spent inputs across restart;
  - stale provider refreshes failing to overwrite newer local hidden sets;
  - atomic multi-role registry commits;
  - script cache subtracting mempool-spent and local pending-spent;
  - `queryLatestStateCommitment` using cached healthy topology, refreshing when
    forced or empty, and returning the unique live tail;
  - stale state-commitment eviction followed by a refreshed
    `queryLatestStateCommitment` retry;
  - operator-wallet submission with `scriptCheckouts` preparing one atomic
    registry attempt for operator and script effects;
  - pre-submit failure when a signed body spends a cache-controlled script input
    that was not supplied as a script checkout;
  - `markMissing` hiding stale inputs across refresh;
  - strict stale checkout handling.

### Phase 2: Operator-Main Commit/Scheduler

- Replace `demo/midgard-node/src/operator-wallet-view.ts` with
  `OperatorUtxoCache` for operator-main.
- Add a `UtxoCacheRegistry` Effect service created once at node startup with a
  DB-backed production store.
- Make scheduler refresh and commit share the same `operator-main` cache
  instance from that registry through role-specific `operatorWallet.submitTx`
  facades.
- Production commit/scheduler paths must stop calling `fetchOperatorWalletView`
  directly for wallet completion inputs.
- Route commit/scheduler transaction construction through operator-wallet
  facades so callers receive only `presetWalletInputs`.
- Remove ordinary `.collectFrom([feeInput])` funding from commit/scheduler
  builders and let Lucid balance from cache-controlled `presetWalletInputs`.
- The facade must check out every wallet input candidate exposed to Lucid
  completion, inspect the exact signed transaction body, and fail before submit
  if Lucid spent an un-checked-out operator input.
- Before submit, the facade prepares a durable registry submission attempt with
  effects extracted from the exact signed transaction body.
- After accepted/observed submit, the facade promotes the attempt and exposes
  produced outputs only under selection policy.
- On stale operator-wallet input errors, mark the referenced input missing,
  refresh, and rebuild once.

### Phase 3: Merge Wallet

- Create a separate operator-merge cache instance keyed by merge wallet role.
- The same `UtxoCacheRegistry` service owns both `operator-main` and
  `operator-merge`; cache roles must remain distinct.
- Expose merge wallet completion inputs through an `operator-merge` wallet
  facade.
- Production merge paths must stop calling `fetchOperatorWalletView` directly
  for wallet completion inputs.
- Remove ordinary `.collectFrom([feeInput])` funding from merge builders and let
  Lucid balance from cache-controlled `presetWalletInputs`.
- The merge facade must check out every merge wallet candidate exposed to Lucid
  completion, inspect the exact signed transaction body, and fail before submit
  if Lucid spent an un-checked-out operator-merge input.
- Prepare/promote/reject merge submission attempts through the facade using the
  exact signed transaction body.
- Keep merge maturity/validity logic separate from UTxO caching. The cache only
  prevents stale or duplicate input selection; it must not decide whether a
  block is mergeable.

### Phase 4: Script Caches

- Introduce script caches for state-queue and any script surfaces that are
  repeatedly selected under concurrency.
- Expose `queryLatestStateCommitment` for state-queue callers. It should derive
  the unique latest tail from cached topology when possible and refresh
  chain/mempool state only when needed or forced.
- Do not require callers to pass `purpose` or a `selectCanonicalLiveTail`
  callback. The latest state commitment is a protocol query, not a caller-owned
  selection policy.
- Pass the returned state-commitment checkout into the role-specific
  operator-wallet `submitTx` call through `scriptCheckouts` so operator wallet
  inputs, script inputs, and produced outputs are recorded as one registry
  attempt.
- On stale state-commitment submit failures, mark that outref missing, remove
  it from the spendable cache view, refresh, and rebuild once against
  `queryLatestStateCommitment({ refresh: "always" })`.

### Phase 5: Observability and Runbook

- Emit structured events on refresh, reserve, release,
  submit-attempt-prepared, submit-attempt-promoted, submit-attempt-rejected,
  mark-missing, pending-pruned, and stale-reservation-detected.
- Add readiness details per cache role:
  - known count
  - spendable count
  - reserved count
  - pending-spent count
  - submitting/maybe-accepted tx count
  - missing count
  - last refresh tip/time
  - oldest unresolved reservation age
- Update e2e summary classification so stale-input cache recoveries are visible
  as cache recovery events, not generic recovered submit failures.

## Operator Cache vs Script Cache

| Concern                | Operator cache                                                   | Script cache                                                         |
| ---------------------- | ---------------------------------------------------------------- | -------------------------------------------------------------------- |
| Address type           | Payment credential controlled by service                         | Script credential                                                    |
| Authority              | Local service should be sole spender                             | Other valid spenders may exist                                       |
| Safety source          | Local reservations plus durable submission attempts              | Chain plus mempool plus local attempts                               |
| Produced outputs       | Derived from exact signed tx body by payment credential          | Derived from exact signed/local/mempool tx body and script predicate |
| Refresh behavior       | Chain minus hidden sets, plus accepted/observed local outputs    | Chain plus mempool-produced, minus mempool/local hidden sets         |
| Stale input meaning    | Usually local race, stale provider, or key reuse                 | Normal external invalidation possibility                             |
| Recovery               | Mark missing, refresh, rebuild; investigate repeated local races | Mark missing, refresh script context, rebuild                        |
| Mempool dependency     | Optional diagnostic/evidence                                     | Useful when provider supports it                                     |
| Persistence strictness | High; crash after submit must not reuse inputs                   | High for local effects, but refresh must tolerate external change    |

## Failure Handling Policy

The package should classify stale-input errors but should not hide them. The
caller decides how many rebuild attempts are acceptable.

Recommended Midgard policy:

- One rebuild after marking stale selected inputs missing.
- No silent infinite retry loops.
- Repeated stale operator-owned inputs for the same role should be escalated as
  a cache correctness or duplicate-key deployment bug.
- Repeated stale script inputs should include external invalidation evidence
  from chain/mempool when possible.
- `unknownOutputReferences` for a selected operator wallet input should be
  treated as stale/spent input, not as a commit/merge success recovery signal.
- Ambiguous submit outcomes hide spent inputs but never expose produced outputs
  until tx hash reconciliation provides accepted, observed, or confirmed
  evidence.

## Non-Goals

- No provider-specific transaction submission clients or retry loops in the
  cache package.
- No protocol-specific state-queue, scheduler, DA, or merge rules in the cache
  package.
- No support for old unreleased cache state or current in-repo flow-local wallet
  overlays. Midgard is unreleased; if the cache state shape changes before
  launch, redeploy and delete old local state.
- No production mode that relies on a diagnostic-only provider or test-only
  stale-input workaround.

## Review Status

This design was reviewed by three independent subagents. Each reviewer inspected
the USDCX cache modules directly, compared this document against the observed
semantics, and approved the revised architecture as production-grade and not
over-engineered for Midgard.

Reviewer approvals:

- Reviewer 1: approved after adding durable submitted attempts, accepted vs
  speculative produced-output semantics, and atomic multi-cache persistence.
- Reviewer 2: approved after adding pre-submit durability, stale-query merge
  protection, and advisory-only mempool semantics.
- Reviewer 3: approved after adding Lucid `presetWalletInputs` reservation
  rules, registry service integration, and context-returning submit helper
  requirements.

DevEx pass:

- An independent DevEx review requested changes after finding that the first
  ergonomic layer still leaked too much lifecycle machinery into normal Midgard
  call sites.
- Updated the design around `operatorWallet.submitTx(...)` as the normal
  operator-owned wallet API so commit, scheduler refresh, and merge examples no
  longer expose reservation/lease machinery to ordinary call sites.
- Kept lease-shaped handles (`ReservedOneInputLease`,
  `ReservedManyInputLease`, `toLucidPresetWalletInputs`) as internal/advanced
  implementation surfaces, with no multi-input `.primary` shortcut.
- Rejected the normal API shape that requires callers to pass `{ purpose,
select }` for common paths; purpose and default selection belong in facade
  construction.
- Moved the primitive prepare/accepted/maybe-accepted/rejected lifecycle into
  an advanced escape-hatch section and placed raw wallet UTxO reads behind
  `lucidSubmission.unsafe.uncachedPresetWalletInputs`.
