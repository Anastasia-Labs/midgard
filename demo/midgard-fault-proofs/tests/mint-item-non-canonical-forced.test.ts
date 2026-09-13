import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import { computeMidgardNativeTxId } from "@al-ft/midgard-core";
import { deriveMidgardForcedTxProofSource } from "@al-ft/midgard-core/codec/forced";
import {
  ForcedInclusionTxV1Schema,
  forcedVerdictSubject,
  OutputReference,
  Proof,
  ROOT_DOMAINS,
} from "@al-ft/midgard-sdk";
import { Data, getAddressDetails } from "@lucid-evolution/lucid";
import { expect, it } from "vitest";

import {
  submitMintItemNonCanonicalStep01Forced,
  submitMintItemNonCanonicalStep02,
  submitMintItemNonCanonicalStep03,
  submitMintItemNonCanonicalStep04,
} from "../src/mint-item-non-canonical/index.js";
import { buildCountedRoot } from "../src/transition-trace/phas.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import { makeNativeTx } from "./support/emulator/native-tx.js";
import {
  evidenceOf,
  registeredContracts,
  witnessSetCborOf,
} from "./support/mint-item-non-canonical-lifecycle.js";
import { mintField } from "./support/mint-item-vectors.js";
import { buildInvalidForcedTransitionTraceFixture } from "./support/submit-init-emulator-fixtures.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  submitSetupTx,
} from "./support/submit-init-emulator-shared.js";

it("authenticates a wrongly accepted forced mint and refuses a forged forced leaf/direction", async () => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: {
      realMintItemNonCanonical: true,
      alwaysFraudProofCatalogue: true,
    },
  });
  const registered = await registeredContracts(harness);
  const tx = makeNativeTx({
    spendInputCbors: [],
    fee: 0n,
    mintPreimageCbor: mintField(Buffer.from("8240a14005", "hex")),
  });
  const id = computeMidgardNativeTxId(tx).toString("hex");
  const source = deriveMidgardForcedTxProofSource(tx);
  const key = { transactionId: "ab".repeat(32), outputIndex: 0n };
  const value = {
    tx_id: id,
    submitted_source: {
      compact_cbor: source.compactCbor.toString("hex"),
      witness_set_compact_cbor: source.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        source.fieldPreimageLengthsCbor.toString("hex"),
    },
    verdict: "ForcedTxValid",
  } as const;
  const bytes = {
    key: Buffer.from(Data.to(key, OutputReference), "hex"),
    value: Buffer.from(
      Data.to(value as never, ForcedInclusionTxV1Schema as never),
      "hex",
    ),
  };
  const root = await buildCountedRoot(ROOT_DOMAINS.forcedTransactionsV1, [
    bytes,
  ]);
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  await trie.insert(bytes.key, bytes.value);
  const membership = {
    domain: root.domain,
    root: root.root,
    phas_root: root.phasRoot,
    count: root.count,
    key,
    value,
    proof: Data.from(
      (await trie.prove(bytes.key)).toCBOR().toString("hex"),
      Proof,
    ),
  };
  const credential = getAddressDetails(
    await harness.funderLucid.wallet().address(),
  ).paymentCredential;
  if (credential?.type !== "Key") throw new Error("funder key absent");
  const base = await buildInvalidForcedTransitionTraceFixture({
    operatorVkey: credential.hash,
    now:
      alignUnixTimeToEmulatorSlotBoundary(
        harness.funderLucid,
        harness.emulator.now() + 120_000,
      ) - 1,
  });
  const header = {
    ...base.header,
    forcedTransactionsRoot: root.root,
    forcedTransactionCount: root.count,
  };
  const setup = await submitSetupTx({
    lucid: harness.funderLucid,
    contracts: harness.contracts,
    nonceUtxo: harness.nonceUtxo,
    catalogue: registered.catalogue,
    header,
  });
  await registered.publishReferences();
  const init = await registered.init(setup.fraudulentBlockOutRef);
  let threadOutRef = `${init.result.txHash}#${init.result.firstStepOutputIndex}`;
  const evidence = evidenceOf(
    forcedVerdictSubject({
      transactionId: id,
      sourceKey: key,
      rejectionReason: null,
    }),
    tx,
    0,
  );
  const common = () => ({
    ...registered.common(threadOutRef, 0),
    finding: evidence,
  });
  await expectOnchainRefusal(() =>
    submitMintItemNonCanonicalStep01Forced({
      ...common(),
      forcedSource: { header, membership, direction: 1n },
    }),
  );
  await expectOnchainRefusal(() =>
    submitMintItemNonCanonicalStep01Forced({
      ...common(),
      forcedSource: {
        header,
        membership: {
          ...membership,
          value: { ...value, tx_id: "cd".repeat(32) },
        },
        direction: 0n,
      },
    }),
  );
  const bound = await submitMintItemNonCanonicalStep01Forced({
    ...common(),
    forcedSource: { header, membership, direction: 0n },
  });
  threadOutRef = bound.nextThreadOutRef;
  const opened = await submitMintItemNonCanonicalStep02({
    ...registered.common(threadOutRef, 1),
    evidence,
    nativeTxCompactCbor: source.compactCbor.toString("hex"),
    witnessSetCompactCbor: witnessSetCborOf(tx),
  });
  threadOutRef = opened.nextThreadOutRef;
  const scanned = await submitMintItemNonCanonicalStep03({
    ...registered.common(threadOutRef, 2),
    evidence,
    nativeTxCompactCbor: source.compactCbor.toString("hex"),
    witnessSetCompactCbor: witnessSetCborOf(tx),
  });
  expect(scanned.terminal).toBe(true);
  const finalized = await submitMintItemNonCanonicalStep04({
    ...registered.common(scanned.nextThreadOutRef, 3),
    evidence,
    witnessReferenceScripts: harness.witnessReferenceScripts,
  });
  expect(finalized.fraudProofUnit).toBeTruthy();
}, 120_000);
