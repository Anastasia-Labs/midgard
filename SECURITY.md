# Security policy

## Reporting a vulnerability

Send a private report to [info@anastasialabs.io](mailto:info@anastasialabs.io)
with the subject `Midgard security report`. Include the affected revision and
deployment profile, the behavior you observed, reproduction steps, and the
expected impact. Do not include wallet seed phrases or other credentials.

For suspected active exploitation, credential exposure, or an imminent loss or
halt, use the same address with the subject `URGENT Midgard security report`.
Include the deployment identity, affected revision, timestamps and a safe way
to contact you. The published address is the reporting and emergency contact;
the Midgard project owner owns intake at this address. This policy does not
claim continuous monitoring.

Email is the current reporting option. No dedicated encryption key is published
here. Send a short description without sensitive attachments first and request
an agreed encrypted channel before sending details that require one. Verify any
proposed encryption key through the established contact before using it.

Keep vulnerability details out of public issues and pull requests while the
report is being investigated. There is no promised bounty.

## Response targets

We aim to acknowledge a private report within three business days and provide
an initial impact assessment or a request for further information within seven
calendar days. Business days are Monday–Friday in America/Chicago. After
confirming an issue, we agree a mitigation, fix and disclosure plan with the
reporter. For a critical issue, our target is a safe mitigation within 24 hours
of confirmation; for a high-impact issue, within seven calendar days. Our target
for a validated fix is 30 calendar days for critical and high-impact issues and
60 calendar days for other confirmed issues. We provide a progress update at
least every 14 calendar days while a confirmed issue remains open. These are
response targets, not guarantees; an on-chain fix may need a separately reviewed
deployment. We explain delays and continue to coordinate a disclosure date with
the reporter.

## Good-faith research safe harbor

We welcome good-faith security research on Midgard code in systems you own or
are authorized to use. Research within this boundary that follows this policy
is authorized by Anastasia Labs. We will not initiate or support legal action
for accidental good-faith policy violations, or bring anti-circumvention claims
for that research. We waive restrictions in our own service or acceptable-use
terms only to the extent needed for research covered by this policy. This
commitment covers claims under our control and cannot bind independent third
parties or override applicable law. It does not authorize accessing another
person's data or keys, moving their funds, social engineering, interrupting
a public service or chain, or testing someone else's infrastructure. Use an
isolated checkout/emulator and your own accounts for reproduction. Ask the
reporting contact before a test that could affect a live deployment. If you
encounter private data, stop, retain only the minimum evidence and report it
privately. If a third party brings a claim about compliant research, we will
make known that the work complied with this policy.

## Scope

Midgard is an optimistic rollup for Cardano. This policy covers its Aiken
contracts, transaction builders, node and follower, DA committee, watcher,
operational commands, and the interfaces that connect them. The deployment
manifest and profile identify the contracts and parameters of an actual
instance; a development branch or a passing test does not establish release
readiness.

The initial public testnet, P1, uses curated operators and allow-listed fault
provers and DA challengers. Proof submission is manual. Users can submit normal
L2 transactions, deposits, withdrawals, and native-only forced transactions.
Forced transactions allow inline datums and refuse reference scripts, Plutus
witnesses, redeemers, auxiliary data, and an L2 validity upper bound.

## Trust boundaries

User transactions, chain events, transaction material, API requests, and data
retrieved from peers are untrusted. Authentication must bind evidence to the
correct deployment, committed roots, transaction, verdict, and proof stage
before that evidence can authorize a state change.

Curated operators and allow-listed challengers are part of the P1 trust model.
Their permissions must still be enforced at the protocol entry points. A key's
presence on an allow-list does not authenticate unrelated data or permit a
caller to substitute another deployment, subject, or successor.

The DA committee is trusted to retain and serve the exact attested payload;
its signature does not prove ledger validity. Release and admin credentials
have their stated operational authority and must not bypass protocol checks.
Distinct public key hashes do not establish independent human custody.

Cardano providers and content services can be unavailable, stale, rate-limited,
or compromised. An observation must bind the approved chain and deployment,
canonical inclusion and the applicable finality policy. Retrieved bytes must
match their authenticated commitment. Provider availability cannot redefine
transaction validity or silently authorize recovery.

## Required security properties

- Admitted user inputs receive a bounded, truthful outcome. They must not hold
  block production indefinitely or make an honest operator culpable.
- Ledger transitions and replay evidence preserve authenticated roots, value,
  ordering, and the protocol's single-successor rules.
- A proof can remove a block or slash funds only for the authenticated fault it
  proves. Honest and faulty cases require separate validation.
- Operator registration, proof initiation, DA challenges, administrative actions,
  and signing enforce their applicable authorization boundaries.
- Parsing, transaction evaluation, scheduling, retries, and public endpoints
  bound attacker-controlled work without silently changing protocol semantics.
- Recovery preserves durable obligations and distinguishes lack of evidence
  from an authenticated negative result.

## Runtime and operational assumptions

Public transaction submission, evaluation, history and payload reads expose
attacker-controlled work. Body and response sizes, query/page cardinality,
request rates, concurrent work, queues, retries and cache growth need explicit
bounds. Evaluation is advisory and must preserve admission capacity. Small
queries must not trigger unbounded database scans, provider fanout or replay.

Mempool churn, duplicate/conflicting submissions, expensive scripts and malformed
payloads must receive a bounded outcome while eligible L1 obligations continue.
Disk or dependency failure, worker crashes, stale locks, rollback and provider
throttling must preserve durable work and expose the affected hold. Recovery
must use authenticated evidence and preserve retained DA and durable journals.

Public deployment requires TLS ingress, authenticated admin actions, protected
internal services, secrets outside images and redacted logs/errors/diagnostics.
These are required controls. This policy does not establish that a candidate
branch has passed the corresponding startup, load, redaction or incident drills.

## P1 limitations

P1 does not provide permissionless fault-proof coverage of every invalid state.
Curated operator trust remains necessary for Plutus verdicts, fabricated forced
transactions, the documented censorship hole and the excluded proof categories.
The validation dispute game and validation-trace states are unavailable. Proof
submission and re-drive are manual. Strikes rotate shifts; the public profile
sets the ejection threshold out of reach for P1 operation.

A forced exit cannot execute Plutus or satisfy a native script's expiry branch.
Escape hatch and settlement claims are unsupported. There is no bootstrap state
snapshot; joining parties replay from genesis using payloads retained for 120
days and must join while that history is available.

P1 cost accounting differs from Cardano-exact word accounting. The interim
9,215-byte Data cap, the 2^512 integer refusal, the refusal of normal native
scripts available only from reference inputs, and the legacy rejection-code
format remain. Split cadence, hourly commitments, further DA trim and full
permissionless proof coverage are later work that requires a regenesis.

These limits do not exclude unexpected authorization bypasses, loss of funds,
honest-operator conviction, or user-triggerable liveness failures from reporting.
Report a weakness in a claimed control even when its exploit requires a trusted
role. The investigation must distinguish a documented trust assumption from a
new failure of the implementation.
