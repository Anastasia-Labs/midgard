/** The validation-trace dispute runner removing a convicted block whose
 * operator another party has already slashed by its lost-defense receipt,
 * on the emulator. Both defenders of our case stay silent: the runner times
 * each thread out, closes it into the case with its defender's receipt, and
 * closes the case Convicted. Before the runner's L1 view sees the record,
 * another party lands the slash of the block's operator by the receipt of
 * the thread it lost. The runner still removes the block, the operator's
 * bond already gone, and its terminal carries that slash's economics; only
 * then does it slash the successor by its own receipt; while that slash
 * does not build, the run is held under its alarm, naming the successor's
 * bond unlock, and records no terminal until it lands. The terminal's
 * derivation takes the operator's bond end only from the one burn of this
 * case's receipt for that operator. */
import {
  CML,
  credentialToAddress,
  getAddressDetails,
  type UTxO,
} from "@lucid-evolution/lucid";
import { afterAll, beforeAll, describe, expect, it, vi } from "vitest";

import type { ResolvedProverSigner } from "../src/runtime.js";
import { VALIDATION_TRACE_CASE_CONVICTED } from "../src/validation-trace-dispute/adapter.js";
import { captureValidationTraceLostDefenseSlash } from "../src/validation-trace-dispute/lost-defense-slash.js";
import {
  VALIDATION_TRACE_MOVE_UNBUILT,
  validationTraceDefenderDeadlineMs,
} from "../src/validation-trace-dispute/threads.js";
import { validationTraceRunState } from "../src/validation-trace-dispute/waiting.js";
import {
  admitFraudProofRawL1Snapshot,
  deriveFraudProofRawL1CompletedTerminal,
  type FraudProofRawL1FamilyDefinition,
  type FraudProofRawL1HistoryReader,
  fraudProofRawL1HistoryReader,
  fraudProofRawL1SnapshotRequestForFamily,
} from "../src/workflow/index.js";
import type { FraudProofWorkflowTerminal } from "../src/workflow/journal.js";
import type { FraudProofWorkflowRunResult } from "../src/workflow/orchestrator.fraud-proof-workflow-run-result.js";
import { expectStateQueueHeaderOrder } from "./support/submit-init-emulator-fixtures.js";
import { network } from "./support/submit-init-emulator-shared.js";
import { anotherParty } from "./support/validation-trace-dispute-foreign-verify.js";
import {
  awaitLastSlotAtOrBefore,
  defendTwoThreads,
  gameThread,
  type TwoThreadCase,
} from "./support/validation-trace-dispute-one-step-case.js";
import {
  isPendingFor,
  playThreads,
} from "./support/validation-trace-dispute-one-step-play.js";
import { threadOf } from "./support/validation-trace-dispute-scripted-defender.js";

let dispute: TwoThreadCase;
/** The other party's slash of the block's operator, and what it spent. */
let foreign: Readonly<{ txHash: string; spent: readonly string[] }>;
/** The operator's node as the other party's slash found it. */
let operatorNode: UTxO;
let terminal: FraudProofWorkflowTerminal;
/** Whether the ledger's size limit leaves our slash unbuilt. */
let unbuildable = false;
/** Each run while our slash did not build, and whether any was terminal. */
const held: FraudProofWorkflowRunResult[] = [];

const outRefOf = (utxo: Pick<UTxO, "txHash" | "outputIndex">) =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

/** Each defender's lost-defense receipt token. */
const receiptUnit = (unit: string) =>
  dispute.scene.chain.award.policyId + unit.slice(56);

const nodeOf = async (nodeUnit: string) =>
  await dispute.scene.h.proverLucid.utxosAtWithUnit(
    dispute.scene.h.contracts.activeOperators.spendingScriptAddress,
    nodeUnit,
  );

/** The block's terminal as the raw L1 derives it, read by `history`. */
const derive = async ({
  definition,
  history,
}: {
  readonly definition?: (
    base: FraudProofRawL1FamilyDefinition,
  ) => FraudProofRawL1FamilyDefinition;
  readonly history?: (
    base: FraudProofRawL1HistoryReader,
  ) => FraudProofRawL1HistoryReader;
} = {}) => {
  const { scene, removal } = dispute;
  const { authority, releaseFinality, releaseEconomics } = removal!.raw;
  const headerHash = scene.failing.evidence.headerHash;
  const base = removal!.raw.definition(headerHash);
  const bound = definition?.(base) ?? base;
  const request = fraudProofRawL1SnapshotRequestForFamily({
    definition: bound,
    releaseFinality,
  });
  const reader = fraudProofRawL1HistoryReader({
    authority,
    releaseFinality,
    headerHash,
  });
  return await deriveFraudProofRawL1CompletedTerminal({
    snapshot: admitFraudProofRawL1Snapshot({
      value: await authority.capture(request),
      request,
      releaseFinality,
      observationDepth: "inclusion",
    }),
    definition: bound,
    releaseEconomics,
    history: history?.(reader) ?? reader,
  });
};

const facts = (value: FraudProofWorkflowTerminal) => ({
  ...value,
  observedAt: { ...value.observedAt, confirmationDepth: 0 },
});

const REFUSED = /exactly one bond-consuming proof-referenced slash/u;

describe("validation-trace dispute runner: removal of a block whose operator was already slashed by its receipt", () => {
  beforeAll(async () => {
    dispute = await defendTwoThreads({
      withRemoval: true,
      // While `unbuildable`, our slash is built under a size limit it
      // exceeds.
      wrap:
        ({ h }) =>
        (adapter) => ({
          ...adapter,
          preflight: async (context) => {
            if (!unbuildable || !context.action.actionId.includes("slash"))
              return await adapter.preflight(context);
            const parameters = h.emulator.protocolParameters;
            h.emulator.protocolParameters = { ...parameters, maxTxSize: 1_024 };
            await h.proverLucid.switchProvider(h.emulator);
            try {
              return await adapter.preflight(context);
            } finally {
              h.emulator.protocolParameters = parameters;
              await h.proverLucid.switchProvider(h.emulator);
            }
          },
        }),
    });
    const { scene, runner, deployment, a, b, removal } = dispute;
    const headerHash = scene.failing.evidence.headerHash;
    // The L1 view stops before the case closes.
    removal!.holdView();
    await removal!.l1For(headerHash).observe({ headerHash });
    // Both defenders stay silent past their deadlines.
    const deadlines = await Promise.all(
      [a, b].map(async ({ unit }) =>
        validationTraceDefenderDeadlineMs(
          threadOf(await gameThread(scene, unit)),
        ),
      ),
    );
    awaitLastSlotAtOrBefore(scene, Math.max(...deadlines) + 2_000);
    const { history } = await playThreads({
      scene,
      runner,
      sides: [],
      until: async (run) => isPendingFor(run, VALIDATION_TRACE_CASE_CONVICTED),
      limit: 30,
    });
    expect(dispute.prepared.at(-1)?.stage, history.join("\n")).toStrictEqual(
      "close",
    );
    // Another party slashes the block's operator by its receipt.
    const [receipt] = await scene.h.proverLucid.utxosAtWithUnit(
      scene.chain.award.spendingScriptAddress,
      receiptUnit(a.unit),
    );
    [operatorNode] = (await nodeOf(a.defender.nodeUnit)) as [UTxO];
    const lucid = await anotherParty(scene);
    const address = await lucid.wallet().address();
    const signer = {
      source: "direct-seed-phrase",
      address,
      paymentKeyHash: getAddressDetails(address).paymentCredential!.hash,
      selectWallet: () => undefined,
    } as unknown as ResolvedProverSigner;
    const { transaction } = await captureValidationTraceLostDefenseSlash({
      lucid,
      signer,
      deployment,
      removal: removal!.removal,
      headerHash,
      receiptOutRef: outRefOf(receipt!),
    });
    const inputs = transaction.signed.toTransaction().body().inputs();
    foreign = {
      txHash: await transaction.signed.submit(),
      spent: Array.from({ length: inputs.len() }, (_, index) => {
        const input = inputs.get(index);
        return `${input.transaction_id().to_hex()}#${input.index().toString()}`;
      }),
    };
    await lucid.awaitTx(foreign.txHash);
    removal!.releaseView();
    // The removal dates its validity window from the application clock,
    // which on the emulator is the emulator's.
    const clock = vi
      .spyOn(Date, "now")
      .mockImplementation(() => scene.h.emulator.now());
    try {
      const runs: string[] = [];
      unbuildable = true;
      for (let run = 0; run < 12; run += 1) {
        const outcome = await runner.runOnce(scene.failing);
        if (unbuildable && "alarms" in outcome && outcome.alarms !== undefined)
          held.push(outcome);
        // Once our slash has failed to build twice, it builds again.
        if (held.length === 2) unbuildable = false;
        runs.push(
          "reason" in outcome
            ? `${outcome.kind}: ${outcome.reason}`
            : outcome.kind,
        );
        if (
          outcome.kind === "terminal_included" ||
          outcome.kind === "completed"
        ) {
          terminal = outcome.terminal;
          break;
        }
      }
      if (terminal === undefined)
        throw new Error(`the removal did not complete: ${runs.join("\n")}`);
    } finally {
      clock.mockRestore();
    }
  }, 1_800_000);

  afterAll(() => dispute?.recorder?.restore());

  it("closes both silent defenders' threads with their receipts, then the case, and slashes neither before the removal", () => {
    const stages = dispute.prepared.map(({ stage }) => stage);
    const close = stages.indexOf("close");
    expect(
      stages.slice(0, close).filter((stage) => stage === "close_thread"),
    ).toHaveLength(2);
    expect(stages.slice(0, close)).not.toContain("slash_lost_defense");
  });

  it("removes the block with the operator's bond already gone, the terminal carrying that receipt slash's economics", async () => {
    const { scene, a } = dispute;
    const { h } = scene;
    expect(await nodeOf(a.defender.nodeUnit)).toStrictEqual([]);
    expect(foreign.spent).toContain(outRefOf(operatorNode));
    expect(terminal.correction.removalTxHash).not.toBe(foreign.txHash);
    expect(terminal.economics).toMatchObject({
      operatorCredential: a.defender.vkey,
      operatorBondInputOutRef: outRefOf(operatorNode),
      operatorBondInputLovelace: operatorNode.assets.lovelace!.toString(),
    });
    expect(
      terminal.economics.proverRewardOutputOutRef?.startsWith(
        `${foreign.txHash}#`,
      ),
    ).toBe(true);
    const [reward] = await h.proverLucid.utxosByOutRef([
      {
        txHash: foreign.txHash,
        outputIndex: Number(
          terminal.economics.proverRewardOutputOutRef!.split("#")[1],
        ),
      },
    ]);
    expect(reward?.address).toBe(
      credentialToAddress(network, {
        type: "Key",
        hash: h.proverSigner.paymentKeyHash,
      }),
    );
    await expectStateQueueHeaderOrder({
      lucid: h.proverLucid,
      contracts: h.contracts,
      expectedHeaderHashes: [scene.failing.evidence.header.prevHeaderHash],
    });
  });

  it("holds the run under the successor slash's alarm, naming its bond unlock, while that slash does not build, with no terminal until it lands", async () => {
    expect(held).toHaveLength(2);
    for (const run of held) {
      expect(run.kind).toBe("pending");
      expect(validationTraceRunState(run)).toMatchObject({
        namedState: { name: VALIDATION_TRACE_MOVE_UNBUILT, alarm: true },
      });
      if (!("reason" in run)) throw new Error("no reason");
      expect(run.reason.startsWith(VALIDATION_TRACE_MOVE_UNBUILT)).toBe(true);
      expect(run.reason).toContain("the defender's bond unlock");
    }
  });

  it("slashes the successor by its receipt only after the removal, and never the block's operator", async () => {
    const { scene, a, b, prepared } = dispute;
    const slashes = prepared.filter(
      ({ stage }) => stage === "slash_lost_defense",
    );
    expect(slashes).toHaveLength(1);
    expect(prepared.at(-1)!.stage).toBe("slash_lost_defense");
    // The successor's receipt is the one the slash spent.
    const spent = CML.Transaction.from_cbor_hex(slashes[0]!.cbor)
      .body()
      .mint()
      ?.get_assets(CML.ScriptHash.from_hex(scene.chain.award.policyId));
    expect(spent?.get(CML.AssetName.from_hex(b.unit.slice(56)))).toBe(-1n);
    expect(spent?.get(CML.AssetName.from_hex(a.unit.slice(56)))).toBe(
      undefined,
    );
    expect(await nodeOf(b.defender.nodeUnit)).toStrictEqual([]);
  });

  it("derives the same terminal from the raw L1, taking the bond's end only from the burn of this case's receipt for the operator", async () => {
    const { scene, a, b } = dispute;
    const derived = await derive();
    expect(derived).not.toBeNull();
    expect(facts(derived!)).toStrictEqual(facts(terminal));
    // Without the family's receipts, nothing proves the bond's end.
    await expect(
      derive({
        definition: ({ lostDefense: _, ...base }) => base,
      }),
    ).rejects.toThrow(REFUSED);
    // Receipts under another policy name none of this case's.
    await expect(
      derive({
        definition: (base) => ({
          ...base,
          lostDefense: {
            ...base.lostDefense!,
            receiptPolicyId: scene.chain.case.policyId,
          },
        }),
      }),
    ).rejects.toThrow(REFUSED);
    // A case policy the thread never held binds no case.
    await expect(
      derive({
        definition: (base) => ({
          ...base,
          lostDefense: {
            ...base.lostDefense!,
            casePolicyId: scene.chain.award.policyId,
          },
        }),
      }),
    ).rejects.toThrow(REFUSED);
    // The successor's receipt slash consumed another bond.
    await expect(
      derive({
        history: (read) => async () => await read(receiptUnit(b.unit)),
      }),
    ).rejects.toThrow(REFUSED);
    // A transaction that consumed the bond without burning the receipt.
    const unburned: FraudProofRawL1HistoryReader = async (unit) =>
      (
        await fraudProofRawL1HistoryReader({
          ...dispute.removal!.raw,
          headerHash: scene.failing.evidence.headerHash,
        })(unit)
      ).map((transaction) => {
        if (transaction.txHash !== foreign.txHash) return transaction;
        const body = CML.TransactionBody.from_cbor_hex(transaction.bodyCbor);
        const mint = CML.Mint.new();
        const bondUnit = a.defender.nodeUnit;
        mint.set(
          CML.ScriptHash.from_hex(bondUnit.slice(0, 56)),
          CML.AssetName.from_hex(bondUnit.slice(56)),
          -1n,
        );
        body.set_mint(mint);
        return { ...transaction, bodyCbor: body.to_canonical_cbor_hex() };
      });
    await expect(derive({ history: () => unburned })).rejects.toThrow(REFUSED);
    // Two such burns prove no single one.
    await expect(
      derive({
        history: (read) => async (unit) => {
          const transactions = await read(unit);
          return [...transactions, ...transactions];
        },
      }),
    ).rejects.toThrow(REFUSED);
  });
});
