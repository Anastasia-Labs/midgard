/** Source preparation unit: original coordinates and authenticated compact bytes.
 * Physical validator and maximum-carriage acceptance remain separate gates. */
import {
  encodeCbor,
  encodeMidgardSpendInputItem,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES,
  midgardFieldStride,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  type MintingPolicy,
  type UTxO,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence.js";
import { faultProofFieldOpening } from "../src/field-opening.js";
import { inputExistenceArtifact } from "../src/input-existence/artifact.js";
import {
  inputExistenceFieldPlans,
  resolveInputExistenceFieldCarriage,
} from "../src/input-existence/field-carriage.js";
import { prepareInputExistence } from "../src/input-existence/prepare.js";
import { inputExistenceStates } from "../src/input-existence/prepare-step-carriage.js";
import {
  forcedInputExistenceMaterial,
  reopenForcedInputExistence,
} from "../src/input-existence/reopen-forced.js";
import { deriveInputExistenceSelection } from "../src/input-existence/selection.js";
import { openProofThreadSubjectFromCanonicalEvidence } from "../src/input-existence/source.js";
import { checkLedgerAgainstRoot } from "../src/ledger-at.js";
import { inputExistenceFieldCarriageRequirements } from "../src/workflow/input-existence-field-carriage.js";
import { LEDGER_ABSENCE_ARTIFACT } from "../src/workflow/ledger-absence-artifact.js";
import { NO_REFERENCE_INPUT_FAMILY_DEFINITION } from "../src/workflow/no-reference-input.no-reference-input-family-definition.js";
import { NON_EXISTENT_INPUT_FAMILY_DEFINITION } from "../src/workflow/non-existent-input.non-existent-input-family-definition.js";
import { authenticatedHeaderObservation } from "./helpers/canonical-block-evidence-fixture.js";
import { makeNativeTx } from "./support/emulator/native-tx.js";
import { buildDecodingBlockFixture } from "./support/native-script-decoding-emulator.js";

const OWNER = "91".repeat(28);
const input = (outputIndex: number) =>
  encodeMidgardSpendInputItem({ txId: Buffer.alloc(32, 0x11), outputIndex });
const contextFor = async (
  reason: SDK.RejectionReason | null,
  fields: Readonly<{
    spend: readonly Buffer[];
    reference: readonly Buffer[];
  }> = { spend: [input(10), input(30)], reference: [input(20)] },
) => {
  const base = makeNativeTx({
    spendInputCbors: fields.spend,
    fee: 0n,
  });
  const nativeTx = materializeMidgardNativeTxFromCanonical({
    ...base,
    body: {
      ...base.body,
      referenceInputsPreimageCbor: encodeCbor([...fields.reference]),
    },
  });
  const orderKey = { transactionId: "77".repeat(32), outputIndex: 0n };
  const fixture = await buildDecodingBlockFixture({
    operatorVkey: OWNER,
    startTime: 1_700_000_000_999n,
    priorLedgerRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    subject: {
      kind: "forced",
      nativeTx,
      orderKey,
      verdict:
        reason === null ? "ForcedTxValid" : { ForcedTxInvalid: { reason } },
    },
  });
  const block = await canonicalBlockEvidenceFromVerifiedPayload({
    observation: authenticatedHeaderObservation(fixture),
    payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
    daProvenance: {
      trustClass: "public_or_permissionless_da",
      sourceId: "test/input-selection",
      grade: "security",
    },
  });
  return {
    block,
    eventKey: { ForcedTransactionEventKey: { tx_order_id: orderKey } },
  };
};
const sourceFor = async (reason: SDK.RejectionReason | null) =>
  openProofThreadSubjectFromCanonicalEvidence(await contextFor(reason));
const select = (
  source: Awaited<ReturnType<typeof sourceFor>>,
  field: 0 | 1,
  inputIndex?: bigint,
) =>
  deriveInputExistenceSelection({
    source,
    field,
    ...(inputIndex === undefined ? {} : { inputIndex }),
    owner: OWNER,
  });

describe("source-bound input-existence selection", () => {
  it("keeps a spend observation independent of the claimed reference coordinate", async () => {
    const source = await sourceFor({
      InputNotFound: { source_kind: 1n, input_index: 0n },
    });
    const selection = select(source, 0, 0n);
    expect(selection.inputIndex).toBe(0n);
    expect(selection.selectedInput).toEqual({
      tx_id: "11".repeat(32),
      output_index: 10n,
    });
    expect(selection.claimedInput).toEqual({
      tx_id: "11".repeat(32),
      output_index: 20n,
    });
    expect(selection.claimedFieldPlan?.fieldIndex).toBe(1);
    expect(selection.compact.body.spend_inputs_hash).toBe(
      selection.fieldPlan.commitment,
    );
    expect(selection.compact.body.reference_inputs_hash).toBe(
      selection.claimedFieldPlan?.commitment,
    );
    expect(selection.compact.validity_code).toBeNull();
  });
  it("retains raw spend index one while observing reference index zero", async () => {
    const selection = select(
      await sourceFor({ InputNotFound: { source_kind: 0n, input_index: 1n } }),
      1,
      0n,
    );
    expect(selection.selectedInput?.output_index).toBe(20n);
    expect(selection.claimedInput?.output_index).toBe(30n);
    expect(selection.claimedFieldPlan?.fieldIndex).toBe(0);
  });
  it("reuses the same authenticated field for different original indices", async () => {
    const selection = select(
      await sourceFor({ SpendInputSignerMissing: { input_index: 1n } }),
      0,
      0n,
    );
    expect(selection.selectedInput?.output_index).toBe(10n);
    expect(selection.claimedInput?.output_index).toBe(30n);
    expect(selection.claimedFieldPlan).toBeNull();
  });
  it("keeps an explicit same-field observation independent of the leaf's claimed index", async () => {
    const selection = select(
      await sourceFor({ InputNotFound: { source_kind: 0n, input_index: 1n } }),
      0,
      0n,
    );
    expect(selection.inputIndex).toBe(0n);
    expect(selection.selectedInput?.output_index).toBe(10n);
    expect(selection.claimedInput?.output_index).toBe(30n);
    expect(selection.claimedFieldPlan).toBeNull();
  });
  it.each([-1n, 2n])(
    "retains the exact impossible named coordinate %s without an invented item",
    async (inputIndex) => {
      const selection = select(
        await sourceFor({
          InputNotFound: { source_kind: 0n, input_index: inputIndex },
        }),
        0,
      );
      expect(selection.inputIndex).toBe(inputIndex);
      expect(selection.selectedInput).toBeNull();
      expect(selection.claimedInput).toBeNull();
      expect(selection.claimedFieldPlan).toBeNull();
    },
  );
  it("refuses an unrelated impossible observed coordinate", async () => {
    const source = await sourceFor({
      InputNotFound: { source_kind: 1n, input_index: 0n },
    });
    expect(() => select(source, 0, 2n)).toThrow(/only the exact named input/);
  });
  it("requires an independent observed index for accepted verdicts", async () => {
    const source = await sourceFor(null);
    expect(() => select(source, 0)).toThrow(/observed input index is required/);
    expect(select(source, 0, 1n).selectedInput?.output_index).toBe(30n);
  });
  it("refuses observed bytes substituted under the committed compact", async () => {
    const source = await sourceFor({
      InputNotFound: { source_kind: 1n, input_index: 0n },
    });
    expect(() =>
      select(
        {
          ...source,
          fieldPreimages: source.fieldPreimages.map((bytes, index) =>
            index === 0 ? encodeCbor([input(11)]) : bytes,
          ),
        },
        0,
        0n,
      ),
    ).toThrow(/preimage commits to/);
  });
  it("refuses claimed cross-field bytes substituted under the committed compact", async () => {
    const source = await sourceFor({
      InputNotFound: { source_kind: 1n, input_index: 0n },
    });
    expect(() =>
      select(
        {
          ...source,
          fieldPreimages: source.fieldPreimages.map((bytes, index) =>
            index === 1 ? encodeCbor([input(21)]) : bytes,
          ),
        },
        0,
        0n,
      ),
    ).toThrow(/preimage commits to/);
  });
});

describe("input-existence workflow field prerequisites", () => {
  it.each([
    NON_EXISTENT_INPUT_FAMILY_DEFINITION,
    NO_REFERENCE_INPUT_FAMILY_DEFINITION,
  ])("registers both possible source fields for $category", (definition) => {
    expect(definition.fieldCarriage).toHaveLength(2);
  });
  it("prepares both source-bound non-Inline fields and resolves their complete references", async () => {
    const count =
      Math.floor(
        MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES / midgardFieldStride(0),
      ) + 1;
    const fields = {
      spend: Array.from({ length: count }, (_, index) => input(index)),
      reference: Array.from({ length: count }, (_, index) =>
        input(1000 + index),
      ),
    };
    const sourceContext = await contextFor(
      { InputNotFound: { source_kind: 1n, input_index: 0n } },
      fields,
    );
    const prepared = await prepareInputExistence({
      ...sourceContext,
      field: 0,
      inputIndex: 0n,
      owner: OWNER,
      predecessor: await checkLedgerAgainstRoot({
        entries: [],
        utxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        label: "field-plan source predecessor",
      }),
    });
    const artifact = {
      schemaVersion: LEDGER_ABSENCE_ARTIFACT,
      category: "nonExistentInput",
      detectionId: "non-existent-input-wrongful-rejection:0",
      position: 0,
      evidence: inputExistenceArtifact(prepared),
    };
    const address = credentialToAddress("Preprod", {
      type: "Key",
      hash: OWNER,
    });
    const script: MintingPolicy = {
      type: "PlutusV3",
      script: "49480100002221200101",
    };
    const certificateReference: UTxO = {
      txHash: "44".repeat(32),
      outputIndex: 0,
      address,
      assets: { lovelace: 2_000_000n },
      scriptRef: script,
    };
    const requirements = inputExistenceFieldCarriageRequirements(
      (action) => action.input.stage === "step_02",
    );
    const context = {
      signer: { paymentKeyHash: OWNER },
      certificate: { policyId: "55".repeat(28), mintingScript: script },
      references: { fieldPreimageCertificateMint: certificateReference },
    };
    const inputAction = { actionId: "select", input: { stage: "step_02" } };
    const required = await Promise.all(
      requirements.map((requirement) =>
        requirement.requirementForAction(context, {
          action: inputAction,
          artifact,
        }),
      ),
    );
    expect(
      required.map((requirement) => requirement?.planned.fieldIndex),
    ).toEqual([0, 1]);
    for (const requirement of required) {
      expect(requirement?.compactCbor).toBe(prepared.source.compactCbor);
      expect(requirement?.witnessSetCompactCbor).toBe(
        prepared.source.witnessSetCompactCbor,
      );
      expect(requirement?.planned.plan.tier).toBe("RawUtxo");
    }
    const plans = inputExistenceFieldPlans(prepared);
    expect(plans.map((plan) => plan.fieldIndex)).toEqual([0, 1]);
    expect(prepared.compact.body.spend_inputs_hash).toBe(plans[0]?.commitment);
    expect(prepared.compact.body.reference_inputs_hash).toBe(
      plans[1]?.commitment,
    );
    const resolvedFields: number[] = [];
    const fieldReferences = await resolveInputExistenceFieldCarriage({
      prepared,
      resolveField: async (planned) => {
        resolvedFields.push(planned.fieldIndex);
        return {
          publications: planned.plan.publications.map(
            (publication, index): UTxO => ({
              txHash: (planned.fieldIndex === 0 ? "22" : "11").repeat(32),
              outputIndex: index,
              address,
              assets: { lovelace: 2_000_000n },
              datum: SDK.fieldPreimagePublicationDatumCbor(publication.bytes),
            }),
          ),
          certificate: undefined,
        };
      },
    });
    expect(resolvedFields).toEqual([0, 1]);
    expect(fieldReferences.map((utxo) => utxo.txHash)).toEqual([
      "11".repeat(32),
      "22".repeat(32),
    ]);
    const stepReference: UTxO = {
      txHash: "00".repeat(32),
      outputIndex: 0,
      address,
      assets: { lovelace: 2_000_000n },
      scriptRef: script,
    };
    const references = [stepReference, ...fieldReferences];
    const observed = faultProofFieldOpening({
      planned: prepared.fieldPlan,
      referenceInputs: references,
      label: "observed",
    });
    const claimedPlan = prepared.claimedFieldPlan;
    if (claimedPlan === null)
      throw new Error("cross-field fixture has no claimed plan");
    const claimed = faultProofFieldOpening({
      planned: claimedPlan,
      referenceInputs: references,
      label: "claimed",
    });
    expect(observed).toEqual({
      BodyFieldOpening: {
        native_tx_compact_cbor: prepared.source.compactCbor,
        carriage: { RawUtxo: { ref_input_index: 2n } },
      },
    });
    expect(claimed).toEqual({
      BodyFieldOpening: {
        native_tx_compact_cbor: prepared.source.compactCbor,
        carriage: { RawUtxo: { ref_input_index: 1n } },
      },
    });
    expect(prepared.inputIndex).toBe(0n);
    expect(prepared.selectedInput?.output_index).toBe(0n);
    expect(prepared.claimedInput?.output_index).toBe(1000n);
    const observedPublication = fieldReferences[1];
    if (observedPublication === undefined)
      throw new Error("cross-field fixture has no observed publication");
    expect(() =>
      faultProofFieldOpening({
        planned: claimedPlan,
        referenceInputs: [stepReference, observedPublication],
        label: "missing claimed publication",
      }),
    ).toThrow();
  });
});

describe("retained forced evidence on the current shared path", () => {
  it.each([0, 1] as const)(
    "reopens field %s with full compact and authentic counted coordinates",
    async (field) => {
      const sourceContext = await contextFor({
        InputNotFound: { source_kind: BigInt(field), input_index: -1n },
      });
      const prepared = await prepareInputExistence({
        ...sourceContext,
        field,
        owner: OWNER,
        predecessor: await checkLedgerAgainstRoot({
          entries: [],
          utxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
          label: "retained forced predecessor",
        }),
      });
      if (prepared.source.kind !== "forced")
        throw new Error("fixture source is not forced");
      const evidence = {
        headerHash: prepared.source.headerHash,
        forcedSource: {
          header: prepared.source.header,
          membership: prepared.source.membership,
        },
        fullTransactionCbor: prepared.source.fullTransactionCbor,
        inputIndex: prepared.inputIndex,
        eventMembership: prepared.eventMembership,
        transitionMembership: prepared.transitionMembership,
        ledgerMembership: null,
      };
      const opened = reopenForcedInputExistence(evidence, field, OWNER);
      expect(opened.source.compactCbor).toBe(prepared.source.compactCbor);
      expect(opened.source.witnessSetCompactCbor).toBe(
        prepared.source.witnessSetCompactCbor,
      );
      expect(opened.inputIndex).toBe(-1n);
      expect(opened.selectedInput).toBeNull();
      expect(opened.ledgerEvidence).toBe("OutOfDomain");
      expect(
        inputExistenceStates(
          forcedInputExistenceMaterial(evidence, field, OWNER),
        ),
      ).toEqual(inputExistenceStates(opened));
      expect(() =>
        reopenForcedInputExistence(
          {
            ...evidence,
            forcedSource: {
              ...evidence.forcedSource,
              membership: {
                ...evidence.forcedSource.membership,
                count: evidence.forcedSource.membership.count + 1n,
              },
            },
          },
          field,
          OWNER,
        ),
      ).toThrow(/counted root changed/);
      expect(() =>
        reopenForcedInputExistence(
          {
            ...evidence,
            transitionMembership: {
              ...evidence.transitionMembership,
              key: evidence.transitionMembership.key + 1n,
            },
          },
          field,
          OWNER,
        ),
      ).toThrow();
    },
  );
});
