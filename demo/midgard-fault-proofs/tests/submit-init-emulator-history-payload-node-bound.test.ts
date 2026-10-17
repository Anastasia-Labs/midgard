/**
 * History admission at the deployed payload Data-node bound. Every shape the
 * bound admits lands within the operating execution basis, through both
 * admission paths; one node past the bound is refused, and the same payload
 * lands under a deployment whose bound is one node larger.
 */
import { readFileSync } from "node:fs";

import { DEPLOYMENT_MANIFEST_MAX_PAYLOAD_NODES } from "@al-ft/midgard-core/deployment-manifest-identity";
import { replacePlutusConstrFieldCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import { expect, it } from "vitest";

import { realBlueprintPath } from "./support/emulator/blueprints.js";
import {
  historyPairPayloads,
  setupHistoryPair,
} from "./support/emulator/history-pair.js";
import { type CompleteSignedTransactionMeasurement } from "./support/emulator/measurement.js";

const blueprint = SDK.parseFaultProofBlueprint(
  JSON.parse(readFileSync(realBlueprintPath).toString()),
);

/** The deployed payload bounds: the manifest's largest node bound. */
const BOUNDS: SDK.EventHistoryPayloadBounds = {
  inlineLimitBytes: 512n,
  maxPayloadBytes: 15_000n,
  maxPayloadNodes: DEPLOYMENT_MANIFEST_MAX_PAYLOAD_NODES,
};
/** The operating basis every admission must fit, below the L1 maxima. */
const MEMORY_BASIS = 13_200_000n;
const CPU_BASIS = 8_000_000_000n;

const hex = (value: number, bytes: number) =>
  value.toString(16).padStart(2 * bytes, "0");
/** A minimal CBOR head of major type `major` with argument `value`. */
const head = (major: number, value: number) =>
  value < 24
    ? hex(major * 32 + value, 1)
    : value < 256
      ? hex(major * 32 + 24, 1) + hex(value, 1)
      : hex(major * 32 + 25, 1) + hex(value, 2);
/** A canonical list of `items` (already encoded). */
const list = (items: readonly string[]) =>
  items.length === 0 ? "80" : `9f${items.join("")}ff`;

/** Datums of exactly `nodes` Data nodes, one per shape. */
const DATUMS = {
  "nested lists": (nodes: number) =>
    "9f".repeat(nodes - 1) + "80" + "ff".repeat(nodes - 1),
  "nested constructors": (nodes: number) =>
    "d8799f".repeat(nodes - 1) + "d87980" + "ff".repeat(nodes - 1),
  "flat list": (nodes: number) => list(Array(nodes - 1).fill("00")),
  // A list around one map: two nodes per entry.
  "wide map": (nodes: number) => {
    if (nodes % 2 !== 0) throw new Error("A wide map datum has an even count");
    const entries = (nodes - 2) / 2;
    return list([head(5, entries) + "0000".repeat(entries)]);
  },
  // A list of 13-byte strings: the most payload bytes at the node bound.
  "long bytes": (nodes: number) =>
    list(Array(nodes - 1).fill(head(2, 13) + "ab".repeat(13))),
} as const;
type Shape = keyof typeof DATUMS;

/** The deposit payload carrying `datum`, as exact payload bytes. */
const depositPayloadCbor = (
  h: Awaited<ReturnType<typeof setupHistoryPair>>,
  nonce: UTxO,
  datum: string,
) => {
  const payload = historyPairPayloads(h)[0]!;
  if (!("DepositPayload" in payload))
    throw new Error("Expected a deposit payload");
  payload.DepositPayload.event.id = {
    transactionId: nonce.txHash,
    outputIndex: BigInt(nonce.outputIndex),
  };
  payload.DepositPayload.event.info.l2_datum = 0n;
  return replacePlutusConstrFieldCbor(
    Data.to(payload, SDK.EventHistoryPayload),
    [0, 1, 2, 0],
    datum,
  );
};

/** Payload nodes besides the datum, from the SDK's own count. */
const nodesAroundDatum = (payloadCbor: string) =>
  Number(SDK.countHistoryDataNodesCbor(payloadCbor, 1_000_000n)) - 1;

type Row = {
  readonly name: string;
  readonly shape: Shape;
  /** Payload nodes minus the deployed bound. */
  readonly over: number;
  readonly path: "InsertOrder" | "PromoteFiller";
  /** The node bound the deployment applies, when not the deployed one. */
  readonly deploymentNodes?: bigint;
  readonly admitted: boolean;
  /** Fill the inline limit instead of the node bound. */
  readonly inline?: boolean;
};

const AT_BOUND: readonly Row[] = (
  [
    "nested lists",
    "nested constructors",
    "flat list",
    "wide map",
    "long bytes",
  ] as const
).flatMap((shape) =>
  (["InsertOrder", "PromoteFiller"] as const).map((path) => ({
    name: `${path} ${shape} at the node bound`,
    shape,
    over: 0,
    path,
    admitted: true,
  })),
);

const ROWS: readonly Row[] = [
  ...AT_BOUND,
  {
    name: "InsertOrder inline flat list filling the inline limit",
    shape: "flat list",
    over: 0,
    path: "InsertOrder",
    admitted: true,
    inline: true,
  },
  ...(["InsertOrder", "PromoteFiller"] as const).map((path) => ({
    name: `${path} nested lists one node past the bound`,
    shape: "nested lists" as const,
    over: 1,
    path,
    admitted: false,
  })),
  {
    name: "InsertOrder nested lists one node past the bound under a bound one larger",
    shape: "nested lists",
    over: 1,
    path: "InsertOrder",
    deploymentNodes: BOUNDS.maxPayloadNodes + 1n,
    admitted: true,
  },
];

it.each(ROWS)(
  "$name: admitted=$admitted",
  async (row) => {
    const records: {
      label?: string;
      measurement?: CompleteSignedTransactionMeasurement;
    }[] = [];
    const h = await setupHistoryPair({
      blueprint,
      records,
      bounds: {
        ...BOUNDS,
        maxPayloadNodes: row.deploymentNodes ?? BOUNDS.maxPayloadNodes,
      },
    });
    // The deposit list holds a filler under the first event nonce's key and
    // none under the second's, so they select promotion and insertion.
    const nonce = h.eventNonces[row.path === "PromoteFiller" ? 0 : 1]!;
    const others = nodesAroundDatum(depositPayloadCbor(h, nonce, "00"));
    let datumNodes = Number(BOUNDS.maxPayloadNodes) + row.over - others;
    if (row.inline === true) {
      const fits = (nodes: number) =>
        BigInt(
          depositPayloadCbor(h, nonce, DATUMS[row.shape](nodes)).length / 2,
        ) <= BOUNDS.inlineLimitBytes;
      datumNodes = 1;
      while (fits(datumNodes + 1)) datumNodes += 1;
    }
    const payloadCbor = depositPayloadCbor(
      h,
      nonce,
      DATUMS[row.shape](datumNodes),
    );
    const payloadNodes = SDK.countHistoryDataNodesCbor(payloadCbor, 1_000_000n);
    expect(payloadNodes).toBe(BigInt(datumNodes + others));
    if (row.inline !== true)
      expect(payloadNodes).toBe(BOUNDS.maxPayloadNodes + BigInt(row.over));
    const applied = h.applied[0]!;
    // The builder's own bound check runs off chain; a refused row lifts it so
    // that the transaction reaches the deployed validator.
    const recipe = {
      ...h.recipes[0]!,
      maxPayloadNodes: payloadNodes,
    };
    const context = {
      lucid: h.lucid,
      applied,
      recipe,
      hubReference: h.hub,
      scriptReference: h.scripts[0]!,
      fundingInputs: await h.funding(),
    };
    const reclaimAuth: SDK.CredentialD = { PublicKeyCredential: [h.owner] };
    const plan = SDK.prepareEventHistoryPayloadCbor(
      payloadCbor,
      reclaimAuth,
      recipe,
    );
    expect(plan.kind).toBe(row.inline === true ? "Inline" : "External");
    let externalData: UTxO | undefined;
    if (plan.kind === "External") {
      const publication = await SDK.buildEventHistoryPublication(
        context,
        payloadCbor,
        reclaimAuth,
        h.emulator.now() + 60_000,
      );
      const txHash = await h.submit("publish-payload", publication.tx);
      [externalData] = await h.lucid.utxosByOutRef([
        { txHash, outputIndex: publication.publicationOutputIndex },
      ]);
    }
    h.emulator.awaitSlot(100);
    context.fundingInputs = await h.funding();
    const listBefore = await h.lucid.utxosAt(applied.address);
    const request = () =>
      SDK.buildEventHistoryAdmission(context, {
        payloadCbor,
        reclaimAuth,
        nonce,
        assets: { lovelace: 25_000_000n },
        structuralLovelace: 5_000_000n,
        structuralRefundKey: h.owner,
        ...(externalData === undefined ? {} : { externalData }),
        validFrom: h.emulator.now() - 60_000,
        validTo: h.emulator.now() + 10_000,
      });
    if (!row.admitted) {
      await expect(request()).rejects.toThrow();
      expect(await h.lucid.utxosAt(applied.address)).toEqual(listBefore);
      expect(await h.lucid.utxosByOutRef([nonce])).toHaveLength(1);
      return;
    }
    const admission = await request();
    expect(admission.anchor.key === admission.plan.key).toBe(
      row.path === "PromoteFiller",
    );
    await h.submit("admission", admission.tx);
    const measured = records
      .filter(({ label }) => label === "admission")
      .at(-1)!.measurement!;
    expect(measured.executionMemory).toBeLessThanOrEqual(MEMORY_BASIS);
    expect(measured.executionSteps).toBeLessThanOrEqual(CPU_BASIS);
    const witness = await SDK.fetchEventHistoryWitness(
      h.lucid,
      {
        policyId: applied.policyId,
        address: applied.address,
        retentionAddress: applied.retention.address,
        inlineLimitBytes: recipe.inlineLimitBytes,
      },
      { transactionId: nonce.txHash, outputIndex: BigInt(nonce.outputIndex) },
    );
    if (witness.kind !== "Present") throw new Error("Admitted order missing");
    expect(
      SDK.captureEventHistoryWitness(witness, applied.policyId, "Deposit")
        .payloadCbor,
    ).toBe(plan.payloadCbor);
    expect(await h.lucid.utxosByOutRef([nonce])).toHaveLength(0);
  },
  240_000,
);
