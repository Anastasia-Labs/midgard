/** The validation-trace dispute runner from Init to conviction at the
 * deepest trace admission takes, on the emulator, for one redeemer field
 * length and one CEK arm. The failing transaction's script runs to the
 * per-transaction memory cap before it fails, so our trace is as long as a
 * real failing event's can be. Two operators defend, the most the public
 * deployments curate: the operator agrees with our trace up to the last CEK
 * step of the arm, so its thread takes the bisection's longest path, and
 * the successor up to the redeemer field's canonical-decode step. The runner
 * bisects both threads, resolves each in its one-step resolution, closes
 * them into our case and, past the case's defense deadline, closes it
 * Convicted with our bond back; every move of ours lands within the
 * transaction limits at its real fee, the thread paying it under the
 * per-move cap and our wallet over it. */
import { validationTraceDepthForStepCount } from "@al-ft/midgard-core";
import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import type { MidgardVersionedScript } from "@al-ft/midgard-core/codec/versioned-script";
import * as SDK from "@al-ft/midgard-sdk";
import { plutusV3ScriptWitness } from "@al-ft/midgard-validation/tests/validation-fixtures";
import {
  CML,
  credentialToAddress,
  Data,
  type UTxO,
} from "@lucid-evolution/lucid";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import { confirmedValidationTraceStages } from "../../src/validation-trace-dispute/actions.js";
import {
  VALIDATION_TRACE_CASE_CONVICTED,
  VALIDATION_TRACE_CASE_OPEN,
} from "../../src/validation-trace-dispute/adapter.js";
import { VALIDATION_TRACE_MOVE_FEE_CAP_LOVELACE } from "../../src/validation-trace-dispute/move-fee.js";
import { VALIDATION_TRACE_INLINE_OBSERVE_MAX_PREIMAGE_BYTES } from "../../src/validation-trace-dispute/one-step-canonical-decode.js";
import type { CekCoreGroup } from "../../src/validation-trace-dispute/one-step-core-chains.js";
import { validationTraceOneStepHolder } from "../../src/validation-trace-dispute/one-step-holders.js";
import { validationTraceDefenseDeadlineMs } from "../../src/validation-trace-dispute/waiting.js";
import { parseOwnOutRef } from "../../src/workflow/own-outputs.js";
import { ADMITTED_TX_EX_UNITS } from "../helpers/plutus-execution-block-fixture.js";
import { network } from "./submit-init-emulator-shared.js";
import {
  expectRealMoveFee,
  landedMoveFee,
} from "./validation-trace-dispute-move-fees.js";
import { copiesOf } from "./validation-trace-dispute-observe-copies.js";
import {
  awaitLastSlotAtOrBefore,
  defendTwoThreads,
  liveCase,
  threadClosed,
  type TwoThreadCase,
} from "./validation-trace-dispute-one-step-case.js";
import { lastCoreStepOf } from "./validation-trace-dispute-one-step-cek-builtin-drive.js";
import {
  confirmedOneStepMoves,
  isPendingFor,
  playThreads,
} from "./validation-trace-dispute-one-step-play.js";
import {
  BOND_LOVELACE,
  type Scene,
  setUpScene,
} from "./validation-trace-dispute-open-scene.js";
import { redeemerDataForFieldPreimage } from "./validation-trace-dispute-redeemer-field.js";
import { makeRunner } from "./validation-trace-dispute-runner.js";
import {
  liveThread,
  threadOf,
} from "./validation-trace-dispute-scripted-defender.js";
import { measureWithinTransactionLimits } from "./validation-trace-dispute-transaction-limits.js";

/**
 * A failing PlutusV3 script that runs as long as admission allows before it
 * fails: `LONGEST_PASSING_SCRIPT` with `(error)` in place of its result,
 * `(program 1.1.0 (lam ctx [(lam _ (error)) (countdown 5148 0)]))`; in Flat,
 * wrapped in one CBOR byte string.
 */
export const LONGEST_FAILING_SCRIPT = plutusV3ScriptWitness(
  encodeCbor(
    Buffer.from(
      "01010023263332323002233002002001230022330020020012225333573466e1c00920001001133003337020049001000a4170a0900001",
      "hex",
    ),
  ),
);

/** The last agreed state before the canonical-decode step of the failing
 * transaction's redeemer field. */
const REDEEMER_FIELD_STEP_FROM = 8;

export const TIMEOUT = 3_600_000;

/**
 * The failing scripts whose traces end in a CEK builtin step of each
 * builtin chain shape, as long as admission allows: `(program 1.1.0 (lam
 * ctx [(lam _ TAIL) (countdown 5147 0)]))`, one countdown step short of
 * `LONGEST_FAILING_SCRIPT` so the tail fits, with TAIL
 *
 * - semantic: `[(lam _ [(lam _ (error)) [(builtin unMapData) (Map [])]])
 *   [(builtin iData) 5]]`, a semantic builtin and a map conversion's start;
 * - failure: `[(builtin divideInteger) 1 0]`, a direct builtin's failure;
 * - blsUncompress: `[(builtin bls12_381_G2_uncompress) #937f..44a7]`, a
 *   direct builtin's failure at the arm's costliest shape: a G2 point on the
 *   twist, outside G2, whose y^2 has a u part.
 *
 * Every trace takes the countdown's direct scalar builtin steps.
 */
export const LONGEST_FAILING_BUILTIN_SCRIPTS = Object.freeze({
  semantic: plutusV3ScriptWitness(
    encodeCbor(
      Buffer.from(
        "010100232323263756980101a000375090051999191801119801001000918011198010010009112999ab9a3370e0049000080089980199b810024800800520b650480001",
        "hex",
      ),
    ),
  ),
  failure: plutusV3ScriptWitness(
    encodeCbor(
      Buffer.from(
        "01010023233706900124000666464600446600400400246004466004004002444a666ae68cdc380124000200226600666e040092002001482d94120001",
        "hex",
      ),
    ),
  ),
  blsUncompress: plutusV3ScriptWitness(
    encodeCbor(
      Buffer.from(
        "010100232378491160937f9ebde4497e13a660d4e184d974218b21d18c9d578ab770360ad3fe41a4217e4ac78c554e05bfd2fc02336b84d90505974a366247ac98e953e9afec7f10dd12080a46e6f7dede942d6265721ee3c807009433d1bc69bf82d69569713244a7003332323002233002002001230022330020020012225333573466e1c00920001001133003337020049001000a416ca0900001",
        "hex",
      ),
    ),
  ),
});

/** The holders each driven CEK core group's arm chain visits, in order. */
const ARMS_OF = {
  0: ["compute"],
  1: ["machine"],
  3: ["directScalarRoots", "directScalarBudget", "directScalar"],
  5: [
    "semanticRoots",
    "semanticResult",
    "semanticBudget",
    "semanticDataScalar",
  ],
  6: ["mapStartRoots", "mapStartBudget", "mapStartNodes"],
  9: ["failureKnown", "failureBudget"],
  12: ["failureBlsUncompress", "failureBudget"],
} as const satisfies Partial<Record<CekCoreGroup, readonly string[]>>;

type DrivenGroup = keyof typeof ARMS_OF;

/** Where the successor's thread disputes our trace: the redeemer field's
 * canonical-decode step, the field `fieldBytes` long, or the last CEK core
 * step of `group`. */
export type DeepestSuccessor =
  | Readonly<{ fieldBytes: number }>
  | Readonly<{ group: DrivenGroup }>;

/** The address of the first output of the signed transaction `cbor`. */
const firstOutputAddress = (cbor: string) =>
  CML.Address.from_hex(
    CML.Transaction.from_cbor_hex(cbor)
      .body()
      .outputs()
      .get(0)
      .address()
      .to_hex(),
  ).to_bech32();

type Measured = Readonly<{
  bytes: number;
  memory: bigint;
  steps: bigint;
  fee: bigint;
}>;

/**
 * Our case at the deepest trace of `failingScript`, defended on the
 * operator's thread up to the last CEK core step of `group` and on the
 * successor's where `successor` says, played to conviction.
 */
export const deepestCase = ({
  failingScript = LONGEST_FAILING_SCRIPT,
  group,
  successor,
}: {
  readonly failingScript?: MidgardVersionedScript;
  readonly group: DrivenGroup;
  readonly successor: DeepestSuccessor;
}) => {
  const fieldBytes = "fieldBytes" in successor ? successor.fieldBytes : 0;
  const successorAt =
    "fieldBytes" in successor
      ? `the redeemer field ${fieldBytes.toString()} bytes long`
      : `the successor's thread at the trace's last CEK ${ARMS_OF[successor.group][0]} step`;
  /** The holders each thread's resolution must visit. */
  const visited = [
    ...ARMS_OF[group],
    ...("fieldBytes" in successor
      ? ["canonicalDecodeItemObserve" as const]
      : ARMS_OF[successor.group]),
  ];
  let dispute: TwoThreadCase;
  /** The round each thread stood at once it was ReadyForOneStep. */
  const rounds = new Map<string, bigint>();

  describe(`${successorAt}, the operator's thread at the trace's last CEK ${ARMS_OF[group][0]} step`, () => {
    beforeAll(async () => {
      dispute = await defendTwoThreads({
        withRemoval: true,
        builtins: true,
        failingScript,
        failingExUnits: ADMITTED_TX_EX_UNITS,
        operatorFrom: lastCoreStepOf(group),
        ...("fieldBytes" in successor
          ? {
              canonicalDecode: true,
              failingRedeemerData: redeemerDataForFieldPreimage(
                fieldBytes,
                ADMITTED_TX_EX_UNITS,
              ),
              successorFrom: REDEEMER_FIELD_STEP_FROM,
            }
          : { successorFrom: lastCoreStepOf(successor.group) }),
      });
    }, TIMEOUT);

    afterAll(() => dispute?.recorder?.restore());

    it("disputes a script that ran to the per-transaction memory cap before it failed", () => {
      const { terminal, stepCount } = dispute.material;
      expect(terminal.executionMemory).toBeLessThanOrEqual(
        ADMITTED_TX_EX_UNITS[0],
      );
      expect(terminal.executionMemory).toBeGreaterThan(
        ADMITTED_TX_EX_UNITS[0] - 100_000n,
      );
      console.info(
        `trace: ${stepCount.toString()} steps, depth ${validationTraceDepthForStepCount(Number(stepCount)).toString()}, ${terminal.executionMemory.toString()} mem`,
      );
    });

    it(
      "bisects both threads, resolves and closes each, and closes the case Convicted with our bond back",
      async () => {
        const { scene, runner, a, b, deployment, removal } = dispute;
        let pastDeadline = false;
        // The L1 view stops before the play, so after the case's close the
        // runner waits for it to see the record rather than removing the
        // block.
        const { headerHash } = scene.failing.evidence;
        removal!.holdView();
        await removal!.l1For(headerHash).observe({ headerHash });
        const { result, history } = await playThreads({
          scene,
          runner,
          sides: [a, b],
          until: async (run) => {
            for (const { unit } of [a, b]) {
              const utxo = await liveThread(scene, unit);
              if (
                utxo?.address === scene.chain.game.spendingScriptAddress &&
                threadOf(utxo).turn === "ReadyForOneStep"
              )
                rounds.set(unit, threadOf(utxo).round);
            }
            // With both threads closed the case waits for its defense
            // deadline; our clock moves there rather than a block a run, and
            // the next run closes it.
            if (
              isPendingFor(run, VALIDATION_TRACE_CASE_OPEN) &&
              (await threadClosed(dispute, a.unit)) &&
              (await threadClosed(dispute, b.unit))
            ) {
              if (pastDeadline)
                throw new Error(
                  "the case stayed open past its defense deadline",
                );
              awaitLastSlotAtOrBefore(
                scene,
                validationTraceDefenseDeadlineMs(
                  Data.from((await liveCase(dispute)).datum!, SDK.Case),
                ) + 1_000,
              );
              pastDeadline = true;
            }
            return isPendingFor(run, VALIDATION_TRACE_CASE_CONVICTED);
          },
          limit: 400,
        });
        if (!("entries" in result))
          throw new Error(`no workflow journal: ${history.join("\n")}`);
        const depth = BigInt(
          validationTraceDepthForStepCount(Number(dispute.material.stepCount)),
        );
        console.info(
          `rounds: operator ${String(rounds.get(a.unit))}, successor ${String(rounds.get(b.unit))}, depth ${depth.toString()}`,
        );
        // The operator's thread took the bisection's longest path.
        expect(rounds.get(a.unit)).toBe(depth);
        expect(rounds.get(b.unit)).toBeLessThanOrEqual(depth);

        // Each thread resolved through its own one-step resolution and was
        // awarded.
        const moves = confirmedOneStepMoves(result.entries);
        expect(
          moves.filter(({ stage }) => stage === "close_awarded"),
        ).toHaveLength(2);
        const outputs = moves.map(({ txHash }) =>
          firstOutputAddress(scene.l1.signedCbors.get(txHash)!),
        );
        for (const holder of visited)
          expect(outputs).toContain(
            validationTraceOneStepHolder(deployment, holder).address,
          );

        // The case closed Convicted, our bond back.
        const close = confirmedValidationTraceStages(result.entries).at(-1)!;
        expect(close.action.stage).toBe("close");
        const lucid = scene.h.proverLucid;
        expect(
          await lucid.utxosByOutRef([parseOwnOutRef(dispute.bondOutRef)]),
        ).toStrictEqual([]);
        const [refund] = await lucid.utxosByOutRef([
          { txHash: close.txHash, outputIndex: 0 },
        ]);
        expect(refund!.address).toBe(
          credentialToAddress(network, {
            type: "Key",
            hash: scene.h.proverSigner.paymentKeyHash,
          }),
        );
        expect(refund!.assets).toEqual({ lovelace: BOND_LOVELACE });
        if (!("reason" in result)) throw new Error("no reason");
        expect(result.reason).toContain("not yet seen");
      },
      TIMEOUT,
    );

    if ("fieldBytes" in successor)
      it(
        fieldBytes > VALIDATION_TRACE_INLINE_OBSERVE_MAX_PREIMAGE_BYTES
          ? "publishes a copy of the redeemer field for the observe move"
          : "observes the redeemer field inline, publishing no copy",
        () => {
          const copies = copiesOf(dispute.scene, fieldBytes);
          if (fieldBytes > VALIDATION_TRACE_INLINE_OBSERVE_MAX_PREIMAGE_BYTES)
            expect(copies.length).toBeGreaterThan(0);
          else expect(copies).toStrictEqual([]);
        },
      );

    it(
      "lands every move of ours within the transaction limits at its real fee",
      async () => {
        const { scene, prepared, deployment } = dispute;
        // A resolution move is measured by the holder it pays, so each hop
        // of an arm chain has its own row.
        const holderAt = new Map<string, string>([
          [scene.chain.award.spendingScriptAddress, "award"],
          ...(["prepare", "core", "settle", ...visited] as const).map(
            (holder) =>
              [
                validationTraceOneStepHolder(deployment, holder).address,
                holder,
              ] as const,
          ),
        ]);
        const byStage = new Map<string, Measured[]>();
        for (const { stage: moved, hash, cbor } of prepared) {
          if (!scene.l1.signedCbors.has(hash)) continue;
          const fit = measureWithinTransactionLimits(cbor);
          const fee = await landedMoveFee(scene, hash);
          // Under the cap the thread pays the move's real fee; over it our
          // wallet pays.
          if (fee.paid < VALIDATION_TRACE_MOVE_FEE_CAP_LOVELACE)
            expectRealMoveFee(fee);
          else expect(fee.paid).toBeGreaterThanOrEqual(fee.ledger);
          const to = holderAt.get(firstOutputAddress(cbor));
          const stage =
            moved === "resolve" && to !== undefined
              ? `resolve to ${to}`
              : moved;
          byStage.set(stage, [
            ...(byStage.get(stage) ?? []),
            {
              bytes: fit.completeSignedBytes,
              memory: fit.executionMemory,
              steps: fit.executionSteps,
              fee: fee.paid,
            },
          ]);
        }
        expect([...byStage.keys()]).toEqual(
          expect.arrayContaining([
            "reveal",
            "enter_resolution",
            ...visited.map((holder) => `resolve to ${holder}`),
            "close_awarded",
            "close",
          ]),
        );
        const most = <K extends keyof Measured>(rows: Measured[], key: K) =>
          rows.reduce<Measured[K]>(
            (max, row) => (row[key] > max ? row[key] : max),
            rows[0]![key],
          );
        for (const [stage, rows] of byStage)
          console.info(
            `fit ${stage} (${rows.length.toString()} txs, max): bytes=${most(rows, "bytes").toString()} mem=${most(rows, "memory").toString()} steps=${most(rows, "steps").toString()} fee=${most(rows, "fee").toString()}`,
          );
      },
      TIMEOUT,
    );
  });
};

const outRefOf = (utxo: UTxO) =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

/**
 * The failing transaction of `failingScript`, its redeemer `failingRedeemerData`
 * when given, in an honest block, which stamps it TxIsInvalid: the runner
 * starts no workflow and signs nothing, so no operator is slashed.
 */
export const honestBlock = ({
  failingScript,
  failingRedeemerData,
}: {
  readonly failingScript: MidgardVersionedScript;
  readonly failingRedeemerData?: Uint8Array;
}) =>
  describe("the same transaction in an honest block", () => {
    let scene: Scene;

    beforeAll(async () => {
      scene = await setUpScene(undefined, {
        failingScript,
        failingExUnits: ADMITTED_TX_EX_UNITS,
        ...(failingRedeemerData === undefined ? {} : { failingRedeemerData }),
        failingStampedInvalid: true,
      });
    }, TIMEOUT);

    it(
      "starts no workflow and signs nothing",
      async () => {
        const unspent = () =>
          Object.values(scene.h.emulator.ledger)
            .filter(({ spent }) => !spent)
            .map(({ utxo }) => outRefOf(utxo));
        const signed = new Set(scene.l1.signedCbors.keys());
        const before = unspent();
        const result = await makeRunner(scene).runOnce(scene.failing);
        expect(result.kind).toBe("no_fault_detected");
        expect(
          [...scene.l1.signedCbors.keys()].filter((hash) => !signed.has(hash)),
        ).toStrictEqual([]);
        expect(unspent()).toStrictEqual(before);
      },
      TIMEOUT,
    );
  });
