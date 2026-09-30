import {
  computeMidgardNativeTxId,
  decodeMidgardFieldPreimage,
  deriveMidgardNativeTxWitnessSetCompact,
  encodeCbor,
  encodeMidgardNativeScript,
  encodeMidgardTxOutput,
  encodeMidgardVersionedScript,
  hashMidgardVersionedScript,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core";
import {
  encodeMidgardTxInputCanonical,
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
  type MidgardTxInput,
  type NativeTxWitnessSetCompact,
} from "@al-ft/midgard-sdk";
import {
  generateEmulatorAccount,
  getAddressDetails,
  Lucid,
  type Script,
  type UTxO,
} from "@lucid-evolution/lucid";

import type { MissingNativeScriptTxContracts } from "../../src/missing-native-script-tx/contracts.js";
import { resolveProverSigner } from "../../src/runtime.js";
import {
  buildDecodingBlockFixture,
  type DecodingBlockFixture,
} from "./native-script-decoding-emulator.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  makeFaultProofEmulatorHarness,
  makeNativeTx,
  network,
  publishPlainReferenceScriptUtxo,
  submitSetupTx,
} from "./submit-init-emulator-shared.js";

export const missingNativeScriptBytesV1 = (): Buffer =>
  encodeMidgardNativeScript({
    type: "all",
    scripts: [
      {
        type: "sig",
        keyHash: Buffer.from("31".repeat(28), "hex"),
      },
    ],
  });

export const missingVersionedScript = () => {
  const scriptBytes = missingNativeScriptBytesV1();
  return {
    language: "NativeCardano" as const,
    scriptBytes,
    nativeScript: {
      type: "all" as const,
      scripts: [
        {
          type: "sig" as const,
          keyHash: Buffer.from("31".repeat(28), "hex"),
        },
      ],
    },
  };
};

const scriptLockedOutputCbor = ({
  credentialHash,
  keyLocked,
}: {
  readonly credentialHash: string;
  readonly keyLocked: boolean;
}): Buffer =>
  encodeMidgardTxOutput({
    // Testnet enterprise: 0x60 key, 0x70 script.
    address: Buffer.concat([
      Buffer.from([keyLocked ? 0x60 : 0x70]),
      Buffer.from(credentialHash, "hex"),
    ]),
    value: { lovelace: 2_000_000n, assets: new Map() },
  });

const sdkWitnessSet = (tx: MidgardNativeTxFull): NativeTxWitnessSetCompact => {
  const compact = deriveMidgardNativeTxWitnessSetCompact(tx.witnessSet);
  return {
    addr_tx_wits_hash: Buffer.from(compact.addrTxWitsHash).toString("hex"),
    script_tx_wits_hash: Buffer.from(compact.scriptTxWitsHash).toString("hex"),
    redeemer_tx_wits_hash: Buffer.from(compact.redeemerTxWitsHash).toString(
      "hex",
    ),
  };
};

export const makeMissingNativeScriptTxEmulatorHarness = async () => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: {
      realMissingNativeScriptTx: true,
      alwaysFraudProofCatalogue: true,
    },
  });
  const family = harness.contracts.missingNativeScriptTx;
  const category = harness.catalogue.categories.missingNativeScriptTx;
  if (family === undefined || category === undefined) {
    throw new Error("Harness did not build missing-native-script-tx");
  }
  if (
    category.categoryId !==
    FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.missingNativeScriptTx
  ) {
    throw new Error("Unexpected missing-native-script-tx category id");
  }
  const outsider = generateEmulatorAccount({ lovelace: 0n });
  const outsiderLucid = await Lucid(harness.emulator, "Custom");
  outsiderLucid.selectWallet.fromSeed(outsider.seedPhrase);
  const outsiderSigner = resolveProverSigner({
    network,
    walletSeedPhrase: outsider.seedPhrase,
  });
  return { ...harness, family, category, outsiderLucid, outsiderSigner };
};

export type MissingNativeScriptTxFixture = {
  readonly block: DecodingBlockFixture;
  readonly setup: Awaited<ReturnType<typeof submitSetupTx>>;
  readonly producingTx: MidgardNativeTxFull;
  readonly badTx: MidgardNativeTxFull;
  readonly producingTxId: string;
  readonly badTxId: string;
  readonly producingOutputItemCbors: readonly Buffer[];
  readonly badTxSpendInputs: readonly MidgardTxInput[];
  /** Where the accused input landed after the canonical §5.3 sort. */
  readonly badInputIndex: number;
  readonly badTxWitnessSet: NativeTxWitnessSetCompact;
  readonly badTxScriptWitnessItemCbors: readonly Buffer[];
  readonly nativeScriptBytes: Buffer;
  readonly versionedScriptItem: Buffer;
  readonly expectedScriptHash: string;
};

export const setupMissingNativeScriptTxFixture = async ({
  harness,
  scriptPresent = false,
  keyLockedProducingOutput = false,
  decoySpendInputCount = 0,
}: {
  readonly harness: Awaited<
    ReturnType<typeof makeMissingNativeScriptTxEmulatorHarness>
  >;
  readonly scriptPresent?: boolean;
  readonly keyLockedProducingOutput?: boolean;
  /**
   * Extra fabricated spend inputs committed alongside the accused pair, so a
   * test can grow the bad transaction's field-0 preimage past the §8.4
   * tier-1 bound and let size alone select tier-2 carriage.
   */
  readonly decoySpendInputCount?: number;
}): Promise<MissingNativeScriptTxFixture> => {
  const versionedScript = missingVersionedScript();
  const nativeScriptBytes = Buffer.from(versionedScript.scriptBytes);
  const versionedScriptItem = encodeMidgardVersionedScript(versionedScript);
  const expectedScriptHash = hashMidgardVersionedScript(versionedScript);
  const producingOutput = scriptLockedOutputCbor({
    credentialHash: expectedScriptHash,
    keyLocked: keyLockedProducingOutput,
  });
  const keyLockedControlOutput = scriptLockedOutputCbor({
    credentialHash: "99".repeat(28),
    keyLocked: true,
  });
  const producingTx = makeNativeTx({
    spendInputCbors: [],
    fee: 1_000n,
    outputCbors: [producingOutput, keyLockedControlOutput],
  });
  const producingTxId = computeMidgardNativeTxId(producingTx).toString("hex");
  const accusedInput: MidgardTxInput = {
    tx_id: producingTxId,
    output_index: 0n,
  };
  const keyLockedControlInput: MidgardTxInput = {
    tx_id: producingTxId,
    output_index: 1n,
  };
  const decoySpendInputs: readonly MidgardTxInput[] = Array.from(
    { length: decoySpendInputCount },
    (_, index): MidgardTxInput => ({
      tx_id: (index + 1).toString(16).padStart(64, "0"),
      output_index: 0n,
    }),
  );
  const badTxSpendInputs = [
    accusedInput,
    keyLockedControlInput,
    ...decoySpendInputs,
  ].sort((left, right) =>
    Buffer.compare(
      encodeMidgardTxInputCanonical(left),
      encodeMidgardTxInputCanonical(right),
    ),
  );
  const badInputIndex = badTxSpendInputs.findIndex(
    (input) =>
      input.tx_id === accusedInput.tx_id &&
      input.output_index === accusedInput.output_index,
  );
  const badTx = makeNativeTx({
    spendInputCbors: badTxSpendInputs.map(encodeMidgardTxInputCanonical),
    fee: 2_000n,
    scriptTxWitsPreimageCbor: scriptPresent
      ? encodeCbor([versionedScriptItem])
      : encodeCbor([]),
  });
  const badTxId = computeMidgardNativeTxId(badTx).toString("hex");
  const paymentCredential = getAddressDetails(
    await harness.funderLucid.wallet().address(),
  ).paymentCredential;
  if (paymentCredential?.type !== "Key") {
    throw new Error("Funder has no payment key hash");
  }
  const startTime = BigInt(
    alignUnixTimeToEmulatorSlotBoundary(
      harness.funderLucid,
      harness.emulator.now() + 120_000,
    ) - 1,
  );
  const block = await buildDecodingBlockFixture({
    operatorVkey: paymentCredential.hash,
    startTime,
    priorLedgerRoot: "00".repeat(32),
    subject: { kind: "normal", nativeTx: badTx },
    additionalTransactions: [producingTx],
  });
  const setup = await submitSetupTx({
    lucid: harness.funderLucid,
    contracts: harness.contracts,
    nonceUtxo: harness.nonceUtxo,
    catalogue: harness.catalogue,
    header: block.header,
  });
  return {
    block,
    setup,
    producingTx,
    badTx,
    producingTxId,
    badTxId,
    producingOutputItemCbors: [producingOutput, keyLockedControlOutput],
    badTxSpendInputs,
    badInputIndex,
    badTxWitnessSet: sdkWitnessSet(badTx),
    badTxScriptWitnessItemCbors: decodeMidgardFieldPreimage(
      badTx.witnessSet.scriptTxWitsPreimageCbor,
    ),
    nativeScriptBytes,
    versionedScriptItem,
    expectedScriptHash,
  };
};

export const publishMissingNativeScriptTxReferenceScripts = async ({
  lucid,
  contracts,
}: {
  readonly lucid: Parameters<
    typeof publishPlainReferenceScriptUtxo
  >[0]["lucid"];
  readonly contracts: MissingNativeScriptTxContracts;
}): Promise<readonly [UTxO, UTxO, UTxO, UTxO, UTxO, UTxO, UTxO, UTxO]> => {
  const published: UTxO[] = [];
  for (const [index, step] of contracts.steps.entries()) {
    const { utxo } = await publishPlainReferenceScriptUtxo({
      lucid,
      script: step.spendingScript as Script,
      label: `missing-native-script-tx step-0${(index + 1).toString()}`,
    });
    published.push(utxo);
  }
  return published as unknown as readonly [
    UTxO,
    UTxO,
    UTxO,
    UTxO,
    UTxO,
    UTxO,
    UTxO,
    UTxO,
  ];
};

export const fundMissingNativeScriptTxOutsider = async (
  harness: Awaited<ReturnType<typeof makeMissingNativeScriptTxEmulatorHarness>>,
): Promise<void> => {
  // Both of the outsider's addresses are funded. `selectWallet.fromSeed`
  // derives the seed's base address while `resolveProverSigner` derives its
  // enterprise address, and the raw drivers re-select through the signer, so
  // funding only the base address strands every transaction the outsider
  // builds after that call.
  const address = await harness.outsiderLucid.wallet().address();
  const unsigned = await harness.funderLucid
    .newTx()
    .pay.ToAddress(address, { lovelace: 1_000_000_000n })
    .pay.ToAddress(address, { lovelace: 1_000_000_000n })
    .pay.ToAddress(harness.outsiderSigner.address, { lovelace: 1_000_000_000n })
    .pay.ToAddress(harness.outsiderSigner.address, { lovelace: 1_000_000_000n })
    .complete();
  const signed = await unsigned.sign.withWallet().complete();
  await harness.funderLucid.awaitTx(await signed.submit());
};

export type RawAdvanceStep = 3 | 4;
