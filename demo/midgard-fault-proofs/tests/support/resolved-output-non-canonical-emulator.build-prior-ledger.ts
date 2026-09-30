import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  buildMidgardBoundedItem,
  decodeMidgardDatum,
  decodeMidgardLedgerOutputCommitment,
  encodeCbor,
  encodeMidgardLedgerOutputCommitment,
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  materializeMidgardNativeTxFromCanonical,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core";
import {
  AddressData,
  addressDataFromBech32,
  type RejectionReason,
} from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerOutputMaterial } from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import {
  applyResolvedOutputNonCanonicalScripts,
  RESOLVED_OUTPUT_NON_CANONICAL_BLUEPRINT_TITLES,
  type ResolvedOutputCoordinate,
  type ResolvedOutputNonCanonicalContracts,
} from "../../src/resolved-output-non-canonical/index.js";
import { makeFaultProofEmulatorHarness } from "./emulator/harness.js";
import {
  expectRegisteredChainParity,
  familyStepsFromRegisteredChain,
} from "./emulator/registered-chain.js";
import {
  ADVERSARIAL_MEMBERSHIP_PROOF_BRANCH_LEVELS,
  insertAdversarialMembershipSiblings,
} from "./submit-init-emulator-fixtures.js";
import { makeNativeTx } from "./submit-init-emulator-shared.js";

export const network = "Custom" as const;

export const FAMILY = "resolved-output-non-canonical";

export const RESOLVED_OUTPUT_MAXIMUM_BYTES = 16_384;

/** Repeated input items that push field 0/1 into Certified carriage. */
export const MAXIMUM_INPUT_ITEM_COUNT = 800;

export const RESOLVED_OUTPUT_REASON_ARM = "InputSpentOutputNonCanonical";

const address = Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 1)]);

const lovelaceOnly = { lovelace: 2_000_000n, assets: new Map() } as const;

// ---------------------------------------------------------------------------
// Retained prior-ledger output shapes
// ---------------------------------------------------------------------------

/** A canonical two-entry output shorter than one chunk. */
export const smallCanonicalOutput = (): Buffer =>
  encodeMidgardTxOutput({ address, value: lovelaceOnly });

/** The small canonical output followed by one trailing byte. */
export const smallMalformedOutput = (): Buffer =>
  Buffer.concat([smallCanonicalOutput(), Buffer.from([0])]);

const multiAssetValue = () => ({
  lovelace: 2_000_000n,
  assets: new Map(
    [0x0a, 0x0b].map((policyByte) => [
      Buffer.alloc(28, policyByte).toString("hex"),
      new Map([
        ["01", 5n],
        ["0203", 7n],
      ]),
    ]),
  ),
});

const canonicalFourEntryOutput = (scriptBytes: number): Buffer =>
  encodeMidgardTxOutput({
    address,
    value: multiAssetValue(),
    datum: decodeMidgardDatum(Buffer.from(Data.to("ab".repeat(7_000)), "hex")),
    script_ref: {
      language: "PlutusV3",
      scriptBytes: Buffer.alloc(scriptBytes, 0x6b),
    },
  });

/**
 * The largest canonical resolved output the ledger admits: address,
 * multi-asset value, datum payload and reference script, exactly 16,384 bytes.
 * The reference script is the tuning knob; its CBOR header is stable in the
 * range this shape uses, so one correction lands on the exact length.
 */
export const maximumCanonicalOutput = (): Buffer => {
  const probe = canonicalFourEntryOutput(7_000);
  const output = canonicalFourEntryOutput(
    7_000 + (RESOLVED_OUTPUT_MAXIMUM_BYTES - probe.length),
  );
  expect(output).toHaveLength(RESOLVED_OUTPUT_MAXIMUM_BYTES);
  return output;
};

/**
 * The accepted direction's maximum: a canonical four-entry prefix padded with
 * zero bytes to 16,384, so the structural scan walks every stage before the
 * trailing bytes fault it.
 */
export const maximumMalformedOutput = (): Buffer => {
  const prefix = canonicalFourEntryOutput(7_000);
  expect(prefix.length).toBeLessThan(RESOLVED_OUTPUT_MAXIMUM_BYTES);
  return Buffer.concat([
    prefix,
    Buffer.alloc(RESOLVED_OUTPUT_MAXIMUM_BYTES - prefix.length),
  ]);
};

/**
 * The compact descriptor a prior block committed for `output`. A canonical
 * output reconstructs its own descriptor; a malformed one is committed under
 * the small canonical template with the malformed item's length and
 * commitment, which is what an operator who accepted it must have done.
 */
export const descriptorFor = (outputIndex: number, output: Buffer): Buffer => {
  try {
    return buildCanonicalMidgardLedgerOutputMaterial({
      outputIndex,
      outputCbor: output,
    }).descriptorCbor;
  } catch {
    const template = decodeMidgardLedgerOutputCommitment(
      buildCanonicalMidgardLedgerOutputMaterial({
        outputIndex,
        outputCbor: smallCanonicalOutput(),
      }).descriptorCbor,
    );
    return encodeMidgardLedgerOutputCommitment({
      ...template,
      totalLength: output.length,
      itemCommitment: buildMidgardBoundedItem({
        fieldIndex: 2,
        itemIndex: outputIndex,
        bytes: output,
      }).commitment,
    });
  }
};

export type PriorLedgerFixture = Readonly<{
  priorRoot: string;
  priorTxId: string;
  outputIndex: number;
  outRefBytes: Buffer;
  output: Buffer;
  descriptorCbor: Buffer;
  proofCborHex: string;
  /** A sibling key the trie also holds, for membership substitution. */
  siblingProofCborHex: string;
  siblingKeyBytes: Buffer;
  siblingDescriptorCbor: Buffer;
}>;

/**
 * The predecessor ledger: the resolved output at `priorTxId#outputIndex`,
 * one canonical sibling entry, and (at the maximum shape) the adversarial
 * branch siblings that deepen the membership proof.
 */
export const buildPriorLedger = async ({
  output,
  outputIndex = 0,
  adversarialDepth,
  priorTxId = "ab".repeat(32),
}: {
  readonly output: Buffer;
  readonly outputIndex?: number;
  readonly adversarialDepth: boolean;
  readonly priorTxId?: string;
}): Promise<PriorLedgerFixture> => {
  const outRefBytes = encodeMidgardSpendInputItem({
    txId: Buffer.from(priorTxId, "hex"),
    outputIndex,
  });
  const descriptorCbor = descriptorFor(outputIndex, output);
  const siblingKeyBytes = encodeMidgardSpendInputItem({
    txId: Buffer.from("cd".repeat(32), "hex"),
    outputIndex: 1,
  });
  const siblingDescriptorCbor = descriptorFor(1, smallCanonicalOutput());
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  await trie.insert(outRefBytes, descriptorCbor);
  await trie.insert(siblingKeyBytes, siblingDescriptorCbor);
  if (adversarialDepth) {
    await insertAdversarialMembershipSiblings({
      trie,
      targets: [{ key: outRefBytes, domain: 0x2601 }],
      branchLevels: ADVERSARIAL_MEMBERSHIP_PROOF_BRANCH_LEVELS,
    });
  }
  return {
    priorRoot: Buffer.from(trie.hash).toString("hex"),
    priorTxId,
    outputIndex,
    outRefBytes,
    output,
    descriptorCbor,
    proofCborHex: (await trie.prove(outRefBytes)).toCBOR().toString("hex"),
    siblingProofCborHex: (await trie.prove(siblingKeyBytes))
      .toCBOR()
      .toString("hex"),
    siblingKeyBytes,
    siblingDescriptorCbor,
  };
};

// ---------------------------------------------------------------------------
// The challenged transaction
// ---------------------------------------------------------------------------

/**
 * A native transaction whose field 0 (spend inputs) and field 1 (reference
 * inputs) are the supplied out-ref items. Field 1 takes the same 38-byte item
 * form as field 0.
 */
export const buildSubjectTransaction = ({
  spendInputCbors,
  referenceInputCbors = [],
}: {
  readonly spendInputCbors: readonly Buffer[];
  readonly referenceInputCbors?: readonly Buffer[];
}): MidgardNativeTxFull => {
  const base = makeNativeTx({
    spendInputCbors: [...spendInputCbors],
    fee: 7n,
    outputCbors: [],
  });
  if (referenceInputCbors.length === 0) return base;
  return materializeMidgardNativeTxFromCanonical({
    version: base.version,
    validity: base.validity,
    body: {
      ...base.body,
      referenceInputsPreimageCbor: encodeCbor([...referenceInputCbors]),
    },
    witnessSet: base.witnessSet,
  });
};

/** `count` copies of the resolved out-ref in the selected field, the rest empty. */
export const subjectTransactionFor = ({
  sourceKind,
  outRefBytes,
  count,
}: {
  readonly sourceKind: 0 | 1;
  readonly outRefBytes: Buffer;
  readonly count: number;
}): MidgardNativeTxFull => {
  const items = Array.from({ length: count }, () => outRefBytes);
  return buildSubjectTransaction(
    sourceKind === 0
      ? { spendInputCbors: items }
      : { spendInputCbors: [], referenceInputCbors: items },
  );
};

export const resolvedOutputReason = (
  coordinate: ResolvedOutputCoordinate,
): RejectionReason => ({
  InputSpentOutputNonCanonical: {
    source_kind: BigInt(coordinate.sourceKind),
    input_index: BigInt(coordinate.inputIndex),
  },
});

// ---------------------------------------------------------------------------
// Harness, registered chain and block commitment
// ---------------------------------------------------------------------------

export type ResolvedOutputContext = Awaited<
  ReturnType<typeof makeResolvedOutputContext>
>;

export const makeResolvedOutputContext = async () => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: {
      realResolvedOutputNonCanonical: true,
      alwaysFraudProofCatalogue: true,
    },
  });
  const addressData = await Effect.runPromise(
    addressDataFromBech32(
      harness.contracts.fraudProof.spendingScriptAddress,
    ).pipe(Effect.map((value) => Data.from(Data.to(value, AddressData)))),
  );
  const registered =
    harness.contracts.fraudProofContracts.resolvedOutputNonCanonical;
  const category = harness.catalogue.categories.resolvedOutputNonCanonical;
  expectRegisteredChainParity({
    registered,
    applied: applyResolvedOutputNonCanonicalScripts({
      blueprint: harness.realBlueprint,
      network,
      computationThreadPolicyId: harness.contracts.computationThread.policyId,
      fraudProofPolicyId: harness.contracts.fraudProof.policyId,
      fraudProofTokenAddressData: addressData,
      fieldPreimageCertificatePolicyId:
        harness.contracts.fieldPreimageCertificate.policyId,
      hubOracleScriptHash: harness.contracts.hubOracle.spendingScriptHash,
    }),
    category,
  });
  const steps = familyStepsFromRegisteredChain(
    registered.steps,
    RESOLVED_OUTPUT_NON_CANONICAL_BLUEPRINT_TITLES,
  );
  const contracts: ResolvedOutputNonCanonicalContracts = {
    steps,
    computationThread: harness.contracts.computationThread,
    fraudProof: harness.contracts.fraudProof,
    hubOraclePolicyId: harness.contracts.hubOracle.policyId,
    stateQueuePolicyId: harness.contracts.stateQueue.policyId,
    fieldPreimageCertificatePolicyId:
      harness.contracts.fieldPreimageCertificate.policyId,
    fieldPreimageCertificateMintingScript:
      harness.contracts.fieldPreimageCertificate.mintingScript,
  };
  return {
    harness,
    contracts,
    steps,
    catalogue: harness.catalogue,
    category,
  };
};
