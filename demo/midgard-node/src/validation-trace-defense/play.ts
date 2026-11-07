/**
 * The defending operator's transactions, one work item each, signed over
 * the wallet views and journaled (family `dispute_defense`) before their
 * first submit; S6 alone resends them. A key with a live intent builds
 * nothing, so a replacement is only ever built once the attempt before it
 * is dead, and it shares that attempt's input (the case and our node for a
 * Defend, the thread for a move): whichever lands wins.
 *
 * - A Defend spends our active-operator node, as a commit does: it runs
 *   under the L1 control plane and the state-queue mutation lease, never
 *   while a signed commit is unresolved, and holds both until it lands or
 *   its validity has passed (bounded by its validity range and one tick).
 *   It is funded from the operator wallet and the pocket, plain ADA only.
 * - A reveal and an expiry each spend only our thread; they take their
 *   collateral, and any fee the thread does not pay, from the pocket.
 */
import { MIDGARD_VALIDATION_DISPUTE_MAX_VALIDITY_RANGE_MS } from "@al-ft/midgard-core";
import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import {
  buildValidationTraceDefend,
  buildValidationTraceOperatorReveal,
  buildValidationTraceThreadExpiry,
  completeValidationTraceDefend,
  completeValidationTraceMove,
  validationTraceDefendersProof,
  validationTraceMoveWindow,
} from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import type { TxSignBuilder, UTxO } from "@lucid-evolution/lucid";
import { Duration, Effect } from "effect";

import { StateQueueMutationLeasesDB } from "../database/index.js";
import { retrieveUnreconciledSignedSubmission } from "../database/pendingBlockFinalizations.retrieve-finalized-missing-da-payloads.js";
import { Globals, withL1ControlPlane } from "../services/index.js";
import {
  IntentJournal,
  type IntentPlan,
  journaledIntent,
  readIntentStatus,
} from "../services/intent-journal.js";
import { handleSignSubmitNoConfirmation } from "../transactions/utils.await-submitted-transaction-confirmation.js";
import type { ValidationTraceDefenseWork } from "./decide.js";
import {
  readDefenseAnchors,
  readLiveByUnits,
  type ValidationTraceDefensePort,
} from "./deployment.js";
import type { DefendFunding } from "./funding.js";
import { VALIDATION_TRACE_DEFENSE_LEASE_HOLDER } from "./health.js";

/** What playing one item came to. */
export type PlayOutcome =
  | "submitted"
  | "landed"
  | "pending"
  | "commit_in_flight"
  | "busy"
  | "moved"
  | "past_deadline";

type Work<K extends ValidationTraceDefenseWork["kind"]> = Extract<
  ValidationTraceDefenseWork,
  { kind: K }
>;

const promise = <A>(label: string, run: () => Promise<A>) =>
  Effect.tryPromise({
    try: run,
    catch: (cause) =>
      new Error(
        `validation-trace defense: ${label} failed: ${formatUnknownError(cause)}`,
        { cause },
      ),
  });

/** Whether the workflow `key` has a live `dispute_defense` intent now. */
export const liveUnder = (key: string) =>
  Effect.flatMap(IntentJournal, (journal) =>
    journal.liveIntents("dispute_defense", key),
  ).pipe(Effect.map((live) => live.length > 0));

/** Signs over the views, journals and submits; the hash once sent. */
const send = (
  port: ValidationTraceDefensePort,
  unsigned: TxSignBuilder,
  key: string,
  plan: IntentPlan,
  contentRef?: Buffer,
) =>
  handleSignSubmitNoConfirmation(
    port.lucid,
    unsigned,
    journaledIntent("dispute_defense", key, plan, contentRef),
  );

/** The workflow key of a Defend of `caseUnit`. */
export const defendKey = (caseUnit: string) => `defend:${caseUnit}`;

/** The workflow key of `item`: one live attempt per key. */
export const workKey = (item: ValidationTraceDefenseWork): string => {
  switch (item.kind) {
    case "defend":
      return defendKey(item.defenseCase.caseUnit);
    case "reveal":
      return `reveal:${item.thread.unit}:${item.thread.thread.round.toString()}`;
    case "expiry":
      return `expiry:${item.thread.unit}`;
  }
};

/**
 * Waits until `txHash`, journaled under `key`, is no longer live, or
 * `wallMs` has passed; whether it landed.
 */
const holdUntilSettled = (
  port: ValidationTraceDefensePort,
  key: string,
  txHash: string,
  wallMs: number,
) =>
  Effect.gen(function* () {
    const journal = yield* IntentJournal;
    const until = Date.now() + wallMs;
    while (Date.now() < until) {
      const live = yield* journal.liveIntents("dispute_defense", key);
      if (!live.some((intent) => intent.txHash === txHash))
        return (
          (yield* readIntentStatus(txHash).pipe(
            Effect.mapError(
              (cause) =>
                new Error(
                  `validation-trace defense: the status of ${txHash} could not be read: ${formatUnknownError(cause)}`,
                  { cause },
                ),
            ),
          ))?.kind === "landed"
        );
      yield* Effect.sleep(Duration.millis(port.pollMs));
    }
    return false;
  });

/** The Defend of `item`, built, journaled and submitted now. */
const defendNow = (
  port: ValidationTraceDefensePort,
  item: Work<"defend">,
  funding: Extract<DefendFunding, { kind: "funded" }>,
  plan: IntentPlan,
) =>
  Effect.gen(function* () {
    const { defenseCase, material } = item;
    const key = workKey(item);
    if (yield* liveUnder(key)) return "pending" as const;
    if ((yield* retrieveUnreconciledSignedSubmission) !== undefined)
      return "commit_in_flight" as const;
    // Read again under the lease: a commit may have moved our node, or
    // another defender the case, since the survey.
    const anchors = yield* readDefenseAnchors(port);
    const { caseOutput } = yield* readLiveByUnits({
      caseOutput: defenseCase.caseUnit,
    });
    const { activeNode, caseConfig, hubOracle } = anchors;
    if (
      caseOutput === undefined ||
      caseOutput.txHash !== defenseCase.utxo.txHash ||
      caseOutput.outputIndex !== defenseCase.utxo.outputIndex ||
      activeNode === undefined ||
      caseConfig === undefined ||
      hubOracle === undefined
    )
      return "moved" as const;
    const window = validationTraceMoveWindow(port.lucid);
    if (window.validFrom > item.deadline) return "past_deadline" as const;
    const references = yield* port.references;
    const unsigned = yield* promise("the Defend build", async () => {
      const defenders = await validationTraceDefendersProof({
        caseDatum: defenseCase.datum,
        priorDefenders: item.priorDefenders,
        defenderVkh: port.ourVkh,
      });
      const { tx } = buildValidationTraceDefend({
        lucid: port.lucid,
        deployment: {
          network: port.network,
          chain: port.dispute,
          references: { case: references.case },
        },
        caseUtxo: caseOutput,
        caseConfig,
        hubOracle,
        activeNode,
        activeOperatorsReference: references.activeOperators,
        defenderVkh: port.ourVkh,
        defenders,
        defense: {
          root: material.root,
          state: material.stateAtS,
          stateProof: material.proofAtS,
        },
        ...window,
      });
      return await completeValidationTraceDefend({
        tx,
        funding: funding.inputs,
        ...(funding.payPocket ? { pocket: port.pocketAddress } : {}),
      });
    });
    const txHash = yield* send(
      port,
      unsigned,
      key,
      plan,
      Buffer.from(defenseCase.target.headerHash, "hex"),
    );
    yield* Effect.logInfo(
      `validation-trace defense: Defend ${txHash} of ${defenseCase.caseUnit} submitted`,
    );
    // Our node stays spent by the Defend until it lands or dies: hold the
    // lease that long, so no commit builds on the node it spends.
    const l1Now = port.lucid.slotToUnixTime(port.lucid.currentSlot());
    const landed = yield* holdUntilSettled(
      port,
      key,
      txHash,
      Math.max(0, window.validTo - l1Now) + port.tickMs,
    );
    return landed ? ("landed" as const) : ("submitted" as const);
  });

/** The longest a Defend holds the L1 control plane: build, validity, a tick. */
export const validationTraceDefendHoldMs = (tickMs: number) =>
  180_000 + MIDGARD_VALIDATION_DISPUTE_MAX_VALIDITY_RANGE_MS + tickMs;

/**
 * Plays a Defend: nothing while its key has a live intent or a signed
 * commit is unresolved; otherwise under the control plane and the lease.
 */
export const playDefend = (
  port: ValidationTraceDefensePort,
  item: Work<"defend">,
  funding: Extract<DefendFunding, { kind: "funded" }>,
  plan: IntentPlan,
) =>
  Effect.gen(function* () {
    if (yield* liveUnder(workKey(item))) return "pending" as const;
    if ((yield* retrieveUnreconciledSignedSubmission) !== undefined)
      return "commit_in_flight" as const;
    const globals = yield* Globals;
    const result = yield* withL1ControlPlane(
      globals,
      {
        scope: VALIDATION_TRACE_DEFENSE_LEASE_HOLDER,
        maxHoldMs: validationTraceDefendHoldMs(port.tickMs),
      },
      StateQueueMutationLeasesDB.tryWithLease(
        VALIDATION_TRACE_DEFENSE_LEASE_HOLDER,
        () => defendNow(port, item, funding, plan),
        port.lease,
      ),
    );
    return result._tag === "Busy" ? ("busy" as const) : result.value;
  });

/** Our RevealOperator on `item`'s thread, journaled and submitted now. */
export const playReveal = (
  port: ValidationTraceDefensePort,
  item: Work<"reveal">,
  pocket: readonly UTxO[],
  plan: IntentPlan,
) =>
  Effect.gen(function* () {
    const { thread, material } = item;
    const key = workKey(item);
    if (yield* liveUnder(key)) return "pending" as const;
    const { turn } = thread.thread;
    if (typeof turn === "string" || !("AwaitingOperator" in turn))
      return "moved" as const;
    const proof = SDK.validationTraceProofDataFromCore(
      material.proofAt(Number(turn.AwaitingOperator.midpoint)),
    );
    const references = yield* port.references;
    const window = validationTraceMoveWindow(port.lucid);
    const unsigned = yield* promise("the reveal build", () =>
      completeValidationTraceMove({
        lucid: port.lucid,
        stage: "RevealOperator",
        referenced: [references.game],
        ownWallet: { inputs: pocket, changeAddress: port.pocketAddress },
        build: (fee) =>
          buildValidationTraceOperatorReveal({
            lucid: port.lucid,
            deployment: { references: { game: references.game } },
            thread: thread.utxo,
            proof,
            fee,
            ...window,
          }),
      }),
    );
    yield* send(port, unsigned, key, plan);
    return "submitted" as const;
  });

/** The expiry of `item`'s thread, journaled and submitted now. */
export const playExpiry = (
  port: ValidationTraceDefensePort,
  item: Work<"expiry">,
  pocket: readonly UTxO[],
  plan: IntentPlan,
) =>
  Effect.gen(function* () {
    const { thread } = item;
    const key = workKey(item);
    if (yield* liveUnder(key)) return "pending" as const;
    const { caseConfig } = yield* readDefenseAnchors(port);
    if (caseConfig === undefined) return "moved" as const;
    const references = yield* port.references;
    const holder = references[thread.at];
    const window = validationTraceMoveWindow(port.lucid);
    const unsigned = yield* promise("the expiry build", () =>
      completeValidationTraceMove({
        lucid: port.lucid,
        stage: "expiry",
        referenced: [holder, references.case, references.threadExpiry],
        ownWallet: { inputs: pocket, changeAddress: port.pocketAddress },
        build: (fee) =>
          buildValidationTraceThreadExpiry({
            lucid: port.lucid,
            network: port.network,
            holder: thread.at,
            thread: thread.utxo,
            threadUnit: thread.unit,
            caseConfig,
            references: {
              holder,
              case: references.case,
              threadExpiry: references.threadExpiry,
            },
            threadExpiryScriptHash:
              port.dispute.threadExpiry.withdrawalScriptHash,
            fee,
            ...window,
          }),
      }),
    );
    yield* send(port, unsigned, key, plan);
    return "submitted" as const;
  });
