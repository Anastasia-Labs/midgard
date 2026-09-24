/** Genuine admission boundary, separate from synthetic 5,000-byte proof stress. */
import { createHash } from "node:crypto";
import { mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { join } from "node:path";

import * as SDK from "@al-ft/midgard-sdk";
import { type UTxO } from "@lucid-evolution/lucid";
import { afterAll, expect, it } from "vitest";

import { realBlueprintPath } from "./support/emulator/blueprints.js";
import {
  historyPairPayloads,
  setupHistoryPair,
} from "./support/emulator/history-pair.js";
import { EMULATOR_PROTOCOL_PARAMETERS } from "./support/emulator/protocol-parameters.js";

const bytes = readFileSync(realBlueprintPath);
const blueprint = SDK.parseFaultProofBlueprint(JSON.parse(bytes.toString()));
const records: unknown[] = [];
afterAll(() => {
  const directory = process.env.MIDGARD_EVENT_HISTORY_EVIDENCE_DIR;
  if (directory === undefined) return;
  mkdirSync(directory, { recursive: true });
  writeFileSync(
    join(directory, "deposit-admission-asset-bound.json"),
    JSON.stringify(
      {
        scope:
          "Applied genuine history admission at the testnet original-Value asset boundary; native fixture hub governance",
        blueprintSha256: createHash("sha256").update(bytes).digest("hex"),
        protocolParameters: EMULATOR_PROTOCOL_PARAMETERS,
        records,
      },
      (_key, value: unknown) =>
        typeof value === "bigint" ? value.toString() : value,
      2,
    ) + "\n",
  );
});

it.each([
  { mode: "inline", nativeAssets: 9, admitted: true },
  { mode: "external", nativeAssets: 9, admitted: true },
  { mode: "inline", nativeAssets: 10, admitted: false },
  { mode: "external", nativeAssets: 10, admitted: false },
])(
  "$mode deposit original Value: $nativeAssets native assets plus ADA, admitted=$admitted",
  async ({ mode, nativeAssets, admitted }) => {
    const h = await setupHistoryPair({ blueprint, records });
    const assets = Object.fromEntries(
      Array.from({ length: nativeAssets }, (_, i) => [
        h.hubPolicy + i.toString(16).padStart(2, "0"),
        BigInt(i + 1),
      ]),
    );
    await h.submit(
      "mint-deposit-assets",
      await h.lucid
        .newTx()
        .collectFrom(await h.funding())
        .mintAssets(assets)
        .attach.MintingPolicy(h.issuer)
        .addSignerKey(h.owner)
        .complete({ coinSelection: false, localUPLCEval: true }),
    );
    const payload = historyPairPayloads(h)[0]!;
    if (!("DepositPayload" in payload))
      throw new Error("Expected deposit payload");
    payload.DepositPayload.event.info.l2_datum =
      mode === "external" ? "ab".repeat(600) : null;
    const id = payload.DepositPayload.event.id;
    const applied = h.applied[0]!;
    const recipe = h.recipes[0]!;
    const context = {
      lucid: h.lucid,
      applied,
      recipe,
      hubReference: h.hub,
      scriptReference: h.scripts[0]!,
      fundingInputs: await h.funding(),
    };
    const reclaimAuth: SDK.CredentialD = { PublicKeyCredential: [h.owner] };
    let externalData: UTxO | undefined;
    if (mode === "external") {
      const publication = await SDK.buildEventHistoryPublication(
        context,
        payload,
        reclaimAuth,
      );
      const txHash = await h.submit("prepublish-deposit", publication.tx);
      externalData = (
        await h.lucid.utxosByOutRef([
          { txHash, outputIndex: publication.publicationOutputIndex },
        ])
      )[0]!;
    }
    h.emulator.awaitSlot(100);
    const before = await h.lucid.utxosAt(applied.address);
    // Refresh funding after publication; every negative reaches local UPLC evaluation.
    context.fundingInputs = await h.funding();
    const request = () =>
      SDK.buildEventHistoryAdmission(context, {
        payload,
        reclaimAuth,
        nonce: h.eventNonces[0]!,
        assets: { lovelace: 25_000_000n, ...assets },
        structuralLovelace: 5_000_000n,
        structuralRefundKey: h.owner,
        externalData,
        validFrom: h.emulator.now() - 60_000,
        validTo: h.emulator.now() + 10_000,
      });
    if (!admitted) {
      await expect(request()).rejects.toThrow(/failed script execution/u);
      expect(await h.lucid.utxosAt(applied.address)).toEqual(before);
      expect(await h.lucid.utxosByOutRef([h.eventNonces[0]!])).toHaveLength(1);
      records.push({
        label: "over-limit-rejected",
        mode,
        nativeAssets,
        nonceRetained: true,
      });
      return;
    }
    const admission = await request();
    await h.submit("admit-maximum-original-asset-count", admission.tx);
    const witness = await SDK.fetchEventHistoryWitness(
      h.lucid,
      {
        policyId: applied.policyId,
        address: applied.address,
        retentionAddress: applied.retention.address,
        inlineLimitBytes: recipe.inlineLimitBytes,
      },
      id,
    );
    if (witness.kind !== "Present")
      throw new Error("Admitted maximum deposit missing");
    const captured = SDK.captureEventHistoryWitness(
      witness,
      applied.policyId,
      "Deposit",
    );
    expect(captured.originalAssets.get("")?.get("")).toBe(20_000_000n);
    expect(captured.originalAssets.get(h.hubPolicy)?.size).toBe(9);
    expect(captured.originalAssets.has(applied.policyId)).toBe(false);
    expect(witness.retainedDataUtxo !== undefined).toBe(mode === "external");
    expect(await h.lucid.utxosByOutRef([h.eventNonces[0]!])).toHaveLength(0);
    records.push({
      label: "maximum-admitted",
      mode,
      nativeAssets,
      originalAssets: [...captured.originalAssets].map(([policy, names]) => [
        policy,
        [...names],
      ]),
    });
  },
  180_000,
);
