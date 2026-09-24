import { decodeMidgardTxOutput } from "@al-ft/midgard-core";
import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  plutusConstrFieldCbor,
  replacePlutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import {
  buildCanonicalMidgardLedgerEntryOutputMaterial,
  deriveCanonicalOriginalDepositTransitionEffect,
} from "@al-ft/midgard-validation";
import { Data, datumToHash } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { detectTransitionTraceFaults } from "../src/transition-trace/detect.js";
import {
  reopenTransitionDeposit,
  transitionDepositOpening,
} from "../src/transition-trace/history-opening.js";
import { initialTransitionTraceState } from "../src/transition-trace/phases.js";
import { transitionTraceProofChunks } from "../src/transition-trace/proof-carriage.js";
import {
  makeTransitionProofMaterial,
  transitionProofHistorySource,
} from "../src/transition-trace/proof-material.js";
import { reconstructDaPayload } from "../src/transition-trace/reconstruct.js";
import { deriveTransitionTraceReplayEvidence } from "../src/transition-trace/replay.js";
import { transitionTraceYieldData } from "../src/transition-trace/yield-data.js";
import { transitionTraceDepositRetainedFixture } from "./support/transition-trace-retained.js";

const policy = "34".repeat(28);
const id = { transactionId: "56".repeat(32), outputIndex: 2n };
const info: SDK.DepositInfo = {
  l2_address: {
    paymentCredential: { PublicKeyCredential: ["12".repeat(28)] },
    stakeCredential: null,
  },
  l2_network_id: 0n,
  l2_datum: 42n,
};
const payload: SDK.EventHistoryPayload = {
  DepositPayload: { event: { id, info } },
};
const originalAssets: SDK.Value = new Map([
  ["", new Map([["", 3_000_000n]])],
  ["78".repeat(28), new Map([["ab", 17n]])],
]);
const commitment = SDK.eventHistoryCommitment(
  policy,
  "Deposit",
  { event_id: id, inclusion_time: 150n },
  payload,
  originalAssets,
);
const retained = transitionDepositOpening({
  commitment,
  openingCbor: Data.to(
    { payload, original_assets: originalAssets },
    SDK.EventHistoryOpening,
  ),
});
const source = { key: id, value: info };

describe("transition deposit retained opening", () => {
  it.each([true, false])(
    "reconstructs and replays an exact repeated-map deposit; honest=%s",
    async (honest) => {
      const rawDatum = "a3020a010b020c";
      const eventCbor = aikenSerialisedPlutusDataCborPreservingMapOrder(
        replacePlutusConstrFieldCbor(
          Data.to({ id, info }, SDK.DepositEvent),
          [1, 2, 0],
          rawDatum,
        ),
      );
      const rawOpening = replacePlutusConstrFieldCbor(
        retained.openingCbor,
        [0, 0],
        eventCbor,
      );
      const history = transitionDepositOpening({
        commitment: {
          ...commitment,
          payload_hash: datumToHash(
            aikenSerialisedPlutusDataCborPreservingMapOrder(
              plutusConstrFieldCbor(rawOpening, [0]),
            ),
          ),
        },
        openingCbor: rawOpening,
      });
      const fixtures = await transitionTraceDepositRetainedFixture({
        operatorVkey: "89".repeat(28),
        now: 0,
        eventCbor,
        originalAssets: SDK.valueToAssets(originalAssets),
        honest,
      });
      const reconstruct = (fixture: typeof fixtures.current) =>
        reconstructDaPayload({
          payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
          expectedHeaderHash: fixture.headerHash,
          committedHeader: fixture.header,
        });
      const current = await reconstruct(fixtures.current);
      const predecessor = await reconstruct(fixtures.predecessor);
      expect(current.deposits[0]!.valueBytes.toString("hex")).toBe(
        plutusConstrFieldCbor(eventCbor, [1]),
      );
      expect(current.deposits[0]!.valueBytes.toString("hex")).not.toBe(
        SDK.committedDepositValueBytes(current.deposits[0]!.value),
      );
      const replay = await deriveTransitionTraceReplayEvidence({
        current,
        predecessor,
        deposits: [{ history }],
        network: "Custom",
        depositPolicyId: policy,
      });
      expect(replay.depositTransitions).toHaveLength(1);
      const projected = replay.depositTransitions![0]!.projectedUtxo;
      expect(
        decodeMidgardTxOutput(
          Buffer.from(fixtures.current.payload.block_body.utxos[0]![1], "hex"),
        ).datum,
      ).toEqual({ kind: "inline", cbor: Buffer.from(rawDatum, "hex") });
      const operation = deriveCanonicalOriginalDepositTransitionEffect({
        configuredNetwork: "Custom",
        eventId: id,
        l2NetworkId: info.l2_network_id,
        l2Address: info.l2_address,
        l2DatumCbor: Buffer.from(rawDatum, "hex"),
        originalAssets: SDK.valueToAssets(originalAssets),
      }).operations[0];
      if (operation?.type !== "insert")
        throw new Error("Expected exact deposit insertion");
      expect(projected.value).toBe(
        Buffer.from(
          buildCanonicalMidgardLedgerEntryOutputMaterial({
            outRef: operation.outRefCbor,
            outputCbor: operation.outputCbor,
          }).descriptorCbor,
        ).toString("hex"),
      );
      const findings = await detectTransitionTraceFaults(current, replay);
      expect(
        findings.some(
          (finding) =>
            finding.buildable && finding.kind === "invalidOneStepTransition",
        ),
      ).toBe(!honest);
      const finding = findings.find(
        (item) => item.buildable && item.kind === "invalidOneStepTransition",
      );
      if (finding?.buildable) {
        const material = makeTransitionProofMaterial(current, finding.proof);
        const source = transitionProofHistorySource(material);
        expect(source?.valueCbor).toBe(
          current.deposits[0]!.valueBytes.toString("hex"),
        );
        expect(transitionTraceProofChunks(material)).not.toEqual(
          transitionTraceProofChunks(finding.proof),
        );
        expect(
          initialTransitionTraceState(material).proof_commitment.hash,
        ).toBe(transitionTraceProofChunks(material).hash);
        const outputs = transitionTraceYieldData({
          proof: material,
          network: "Custom",
          depositPolicyId: policy,
          depositOpening: history,
        }).find((item) => item.outputCbors !== undefined)!.outputCbors!;
        expect(
          decodeMidgardTxOutput(Buffer.from(outputs[0]!, "hex")).datum,
        ).toEqual({ kind: "inline", cbor: Buffer.from(rawDatum, "hex") });
        expect(() =>
          makeTransitionProofMaterial(
            { ...current, headerHash: "ab".repeat(28) },
            finding.proof,
          ),
        ).toThrow(/header/);
      }
    },
  );
  it("retains raw map pairs and requires the same raw committed source", () => {
    const rawPayload = replacePlutusConstrFieldCbor(
      Data.to(payload, SDK.EventHistoryPayload),
      [0, 1, 2, 0],
      "a3020a010b020c",
    );
    const rawOpening = replacePlutusConstrFieldCbor(
      retained.openingCbor,
      [0],
      rawPayload,
    );
    const rawRetained = transitionDepositOpening({
      commitment: {
        ...commitment,
        payload_hash: datumToHash(
          aikenSerialisedPlutusDataCborPreservingMapOrder(rawPayload),
        ),
      },
      openingCbor: rawOpening,
    });
    const valueCbor = plutusConstrFieldCbor(rawPayload, [0, 1]);
    const rawSource = {
      key: id,
      value: Data.from(valueCbor, SDK.DepositInfo),
      valueCbor,
    };
    expect(
      reopenTransitionDeposit(rawRetained, policy, rawSource).infoCbor,
    ).toBe(valueCbor);
    expect(rawRetained.openingCbor).toBe(rawOpening);
    expect(() =>
      reopenTransitionDeposit(rawRetained, policy, {
        key: id,
        value: rawSource.value,
      }),
    ).toThrow(/source/);
    expect(() =>
      reopenTransitionDeposit(rawRetained, policy, {
        ...rawSource,
        value: info,
      }),
    ).toThrow(/source view/);
  });
  it("reopens exact persisted bytes against the checkpoint without a live pointer", () => {
    const persisted = JSON.parse(JSON.stringify(retained)) as typeof retained;
    const opened = reopenTransitionDeposit(
      persisted,
      policy,
      source,
      retained.commitmentCbor,
    );
    expect(opened.opening.original_assets).toEqual(originalAssets);
    expect(opened.event).toEqual({ id, info });
  });
  it.each([
    [
      "structural ADA",
      new Map([
        ["", new Map([["", 5_000_000n]])],
        ["78".repeat(28), new Map([["ab", 17n]])],
      ]),
    ],
    [
      "changed quantity",
      new Map([
        ["", new Map([["", 3_000_000n]])],
        ["78".repeat(28), new Map([["ab", 18n]])],
      ]),
    ],
    [
      "list NFT",
      new Map([...originalAssets, [policy, new Map([["99".repeat(32), 1n]])]]),
    ],
  ])("rejects an opening containing %s", (_name, assets) => {
    const changed = {
      ...retained,
      openingCbor: Data.to(
        { payload, original_assets: assets },
        SDK.EventHistoryOpening,
      ),
    };
    expect(() => reopenTransitionDeposit(changed, policy, source)).toThrow(
      /commitment/,
    );
  });
  it("rejects different checkpoint time, policy, payload and source", () => {
    const changedCheckpoint = Data.to(
      { ...commitment, inclusion_time: 151n },
      SDK.EventHistoryCommitment,
    );
    expect(() =>
      reopenTransitionDeposit(retained, policy, source, changedCheckpoint),
    ).toThrow(/commitment/);
    expect(() =>
      reopenTransitionDeposit(retained, "35".repeat(28), source),
    ).toThrow(/commitment/);
    expect(() =>
      reopenTransitionDeposit(retained, policy, {
        ...source,
        value: { ...info, l2_datum: 43n },
      }),
    ).toThrow(/source/);
    const changed = {
      ...retained,
      openingCbor: Data.to(
        {
          payload: {
            DepositPayload: { event: { id, info: { ...info, l2_datum: 43n } } },
          },
          original_assets: originalAssets,
        },
        SDK.EventHistoryOpening,
      ),
    };
    expect(() => reopenTransitionDeposit(changed, policy, source)).toThrow(
      /commitment/,
    );
  });
  it("rejects reused IDs whose later admission is not the captured commitment", () => {
    const later = transitionDepositOpening({
      commitment: { ...commitment, inclusion_time: 1000n },
      openingCbor: retained.openingCbor,
    });
    expect(() =>
      reopenTransitionDeposit(later, policy, source, retained.commitmentCbor),
    ).toThrow(/commitment/);
  });
  it("rejects legacy live-outref receipts and malformed deferred data", () => {
    expect(() =>
      reopenTransitionDeposit(
        {
          ...retained,
          commitmentCbor: Data.to([
            Data.from<Data>(Data.to(id, SDK.OutputReference)),
            policy,
            "99".repeat(32),
          ]),
        },
        policy,
        source,
      ),
    ).toThrow();
    expect(() =>
      reopenTransitionDeposit(
        { ...retained, openingCbor: Data.to(42n) },
        policy,
        source,
      ),
    ).toThrow();
  });
  it("projects only authenticated original funds and preserves the submitted datum", () => {
    const effect = deriveCanonicalOriginalDepositTransitionEffect({
      configuredNetwork: "Custom",
      eventId: id,
      l2NetworkId: 0n,
      l2Address: info.l2_address,
      l2DatumCbor: Buffer.from(Data.to(42n), "hex"),
      originalAssets: { lovelace: 3_000_000n, ["78".repeat(28) + "ab"]: 17n },
    });
    const op = effect.operations[0];
    if (op?.type !== "insert")
      throw new Error("Expected original deposit insertion");
    const output = decodeMidgardTxOutput(op.outputCbor);
    expect(output.value.lovelace).toBe(3_000_000n);
    expect(output.value.assets).toEqual(
      new Map([["78".repeat(28), new Map([["ab", 17n]])]]),
    );
    expect(output.datum).toEqual({
      kind: "inline",
      cbor: Buffer.from(Data.to(42n), "hex"),
    });
  });
});
