import "./fault-proof.double-spend-fault-proof-contract-builder.js";

import { readFileSync } from "node:fs";

import { asDataType } from "@al-ft/midgard-core/lucid-data";
import {
  applyParamsToScript,
  Data,
  validatorToAddress,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import * as SDK from "@/index.js";

import { cekCoreEntryHashes } from "../src/fraud-proof/contracts/cek-core.js";
import {
  AddressData,
  addressDataFromBech32,
  buildFaultProofContracts,
  buildInvalidRangeFaultProofContracts,
  buildTransitionTraceFaultProofContracts,
  buildValidationTraceDisputeFaultProofContracts,
  buildZeroInputFaultProofContracts,
  CEK_CONTEXT_STAGE_TITLES,
  CEK_CORE_STAGE_TITLES,
  CEK_PROGRAM_MATERIAL_SPEND_TITLE,
  deriveValidationTraceDeploymentId,
  FAULT_PROOF_SHARED_TITLES,
  fraudProofContractsToFirstSteps,
  INVALID_RANGE_FAULT_PROOF_TITLES,
  TRANSITION_TRACE_FAULT_PROOF_TITLES,
  VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES,
  VALIDATION_TRACE_DISPUTE_STEP_COUNT,
  VALIDATION_TRACE_RESOLVER_COUNT,
  ZERO_INPUT_FAULT_PROOF_TITLES,
} from "../src/index.js";
import {
  blueprintPath,
  CEK_MATERIAL_TRAVERSAL_TITLES,
  CEK_REDEEMER_ITEM_TITLES,
  certificatePolicyId,
  collectTitles,
  compiledScript,
  filterBlueprint,
  h28,
  h28b,
  h28c,
  loadBlueprint,
  MAX_APPLIED_SCRIPT_BYTES,
  spendingScript,
  spendingScriptHash,
} from "./fault-proof.publication-tx-overhead-bytes.js";

describe("fault-proof contract builder", () => {
  it("builds every implemented fault-proof chain from the Aiken blueprint", async () => {
    const blueprint = loadBlueprint();

    const contracts = await Effect.runPromise(
      buildFaultProofContracts({
        eventHistoryBounds: {
          inlineLimitBytes: 512n,
          maxPayloadBytes: 5000n,
          maxPayloadNodes: 512n,
        },
        blueprint,
        network: "Preprod",
        hubOraclePolicyId: h28b,
        fraudProofCataloguePolicyId: h28c,
        referenceScriptAuthPolicyId: h28,
      }),
    );

    expect(contracts.doubleSpend.steps).toHaveLength(4);
    expect(contracts.nonExistentInput.firstStep).toBe(
      contracts.nonExistentInput.steps[0],
    );
    expect(contracts.nonExistentInput.steps).toHaveLength(4);
    expect(contracts.nonExistentInputNoIndex.firstStep).toBe(
      contracts.nonExistentInputNoIndex.steps[0],
    );
    expect(contracts.nonExistentInputNoIndex.steps).toHaveLength(4);
    expect(contracts.referenceInputNoIdx.firstStep).toBe(
      contracts.referenceInputNoIdx.steps[0],
    );
    expect(contracts.referenceInputNoIdx.steps).toHaveLength(4);
    // `reference_input_no_idx` is the reference-input mirror of `input_no_idx`.
    // Steps 01–03 genuinely differ — step 01 commits the bad tx's
    // reference-inputs hash instead of its spend-inputs hash, step 02 opens
    // consensus field 1 with a flat `Args` rather than the spend side's
    // Complete/Published/Fold enum, and the carried source refactor left the
    // step 03 pair compiling to distinct UPLC. Step 04 differs only in record
    // field *names*, which PlutusData erases, so it compiles to the same UPLC
    // and the two chains share that one script — exactly like the
    // `no_input`/`no_reference_input` pair. The threads stay distinguishable
    // because the computation-thread token asset name binds each thread to its
    // own category and block.
    expect(
      new Set([
        ...contracts.referenceInputNoIdx.steps.map(
          (step) => step.spendingScriptHash,
        ),
        ...contracts.nonExistentInputNoIndex.steps.map(
          (step) => step.spendingScriptHash,
        ),
      ]).size,
    ).toBe(7);
    expect(
      contracts.referenceInputNoIdx.steps
        .slice(3)
        .map((step) => step.spendingScriptHash),
    ).toEqual(
      contracts.nonExistentInputNoIndex.steps
        .slice(3)
        .map((step) => step.spendingScriptHash),
    );
    expect(contracts.invalidRange.firstStep).toBe(
      contracts.invalidRange.steps[0],
    );
    expect(contracts.invalidRange.steps).toHaveLength(2);
    expect(contracts.invalidSignature.firstStep).toBe(
      contracts.invalidSignature.steps[0],
    );
    expect(contracts.invalidSignature.steps).toHaveLength(2);
    expect(contracts.zeroInput.firstStep).toBe(contracts.zeroInput.steps[0]);
    expect(contracts.zeroInput.steps).toHaveLength(2);
    expect(contracts.transitionTrace.firstStep).toBe(
      contracts.transitionTrace.steps[0],
    );
    expect(contracts.transitionTrace.steps).toHaveLength(9);

    const history = contracts.transitionTrace.history;
    const depositRetention = SDK.applyEventHistoryRetentionValidator(
      blueprint,
      "Preprod",
      h28b,
      "Deposit",
    );
    const withdrawalRetention = SDK.applyEventHistoryRetentionValidator(
      blueprint,
      "Preprod",
      h28b,
      "Withdrawal",
    );
    expect(history.retentionAddresses).toEqual({
      deposit: depositRetention.address,
      withdrawal: withdrawalRetention.address,
    });
    const depositRetentionData = await Effect.runPromise(
      addressDataFromBech32(depositRetention.address),
    );
    expect(
      contracts.transitionTrace.yields.depositProjection.withdrawalScript
        .script,
    ).toBe(
      applyParamsToScript(
        compiledScript(
          blueprint,
          SDK.TRANSITION_TRACE_YIELD_TITLES.depositProjection,
        ),
        [
          contracts.transitionTrace.finals[5].spendingScriptHash,
          h28b,
          Data.from<Data>(Data.to(depositRetentionData, AddressData)),
          512n,
          5000n,
          512n,
        ],
      ),
    );
    expect(contracts.validationTraceDispute.firstStep).toBe(
      contracts.validationTraceDispute.steps[0],
    );
    expect(contracts.fabricatedDeposit.steps).toHaveLength(4);
    expect(contracts.fabricatedWithdrawal.steps).toHaveLength(4);
    expect(contracts.nativeScriptDecoding.steps).toHaveLength(6);
    expect(contracts.missingSignature.steps).toHaveLength(4);
    expect(contracts.missingNativeScriptTx.steps).toHaveLength(8);
    expect(contracts.withdrawnReferenceInput.steps).toHaveLength(3);
    expect(contracts.canonicalDecodability.steps).toHaveLength(2);
    expect(contracts.committedFieldShape.steps).toHaveLength(2);
    expect(contracts.minFee.steps).toHaveLength(2);
    expect(contracts.withdrawalMistag.steps).toHaveLength(5);
    expect(contracts.doubleWithdraw.steps).toHaveLength(2);
    expect(contracts.crossBlockDuplicateEvent.steps).toHaveLength(2);
    expect(contracts.l2TxMistag.steps).toHaveLength(2);
    expect(contracts.withdrawnInput.steps).toHaveLength(3);
    expect(fraudProofContractsToFirstSteps(contracts)).toMatchObject({
      fabricatedDeposit: contracts.fabricatedDeposit.firstStep,
      missingSignature: contracts.missingSignature.firstStep,
      l2TxMistag: contracts.l2TxMistag.firstStep,
      withdrawnInput: contracts.withdrawnInput.firstStep,
    });

    // Step-script identity, stated as the property the deployment depends on
    // rather than as a folded count of distinct hashes. Two step validators
    // compiling to the same UPLC is legitimate only where the two steps carry
    // the same on-chain logic and differ solely in record field *names*, which
    // PlutusData erases; each such pair is named here so a *new* accidental
    // collision (two steps of one family, or an undeclared cross-family
    // twin) fails, while adding a validator to the surface does not.
    const stepOwners = new Map<string, string[]>();
    for (const [family, value] of Object.entries(
      contracts as unknown as Record<string, unknown>,
    )) {
      const steps = (
        value as { readonly steps?: readonly SDK.SpendingValidator[] }
      ).steps;
      if (!Array.isArray(steps)) {
        continue;
      }
      // No family may spend the same script at two of its own steps: the step
      // index is what advances the dispute, so a repeat would let a prover
      // replay one step in the other's position.
      expect(
        new Set(steps.map((step) => step.spendingScriptHash)).size,
        `${family} step scripts are pairwise distinct`,
      ).toBe(steps.length);
      steps.forEach((step, index) => {
        const owners = stepOwners.get(step.spendingScriptHash) ?? [];
        owners.push(`${family}[${index.toString()}]`);
        stepOwners.set(step.spendingScriptHash, owners);
      });
    }
    expect(
      [...stepOwners.values()].filter((owners) => owners.length > 1).sort(),
    ).toEqual(
      [
        // `no_reference_input` mirrors `non_existent_input` on the reference
        // side; its steps 03/04 differ only in field names.
        ["nonExistentInput[2]", "noReferenceInput[2]"],
        ["nonExistentInput[3]", "noReferenceInput[3]"],
        // `reference_input_no_idx` mirrors `input_no_idx`; only step 04 stayed
        // identical after the carried native-tx view split step 03.
        ["nonExistentInputNoIndex[3]", "referenceInputNoIdx[3]"],
        // The withdrawn spend/reference pair shares its step-03 membership
        // check.
        ["withdrawnReferenceInput[2]", "withdrawnInput[2]"],
        // Both convict from the same final ordered-collection comparison.
        ["mintDeclaredAssetLimit[3]", "observerOrderInvalid[3]"],
        // The unused-witness families share their terminal award step.
        ["unusedScriptWitness[5]", "unusedRedeemer[8]"],
      ].sort(),
    );
  });

  it("builds invalid-range with the validator parameter order from the blueprint", async () => {
    const blueprint = loadBlueprint();

    const contracts = await Effect.runPromise(
      buildInvalidRangeFaultProofContracts({
        blueprint,
        network: "Preprod",
        hubOraclePolicyId: h28b,
        fraudProofCataloguePolicyId: h28c,
      }),
    );

    expect(contracts.invalidRange.firstStep).toBe(
      contracts.invalidRange.steps[0],
    );
    expect(contracts.invalidRange.steps).toHaveLength(2);
    expect(
      new Set(
        contracts.invalidRange.steps.map((step) => step.spendingScriptHash),
      ).size,
    ).toBe(2);

    const fraudProofTokenAddressData = Data.from(
      Data.to(
        await Effect.runPromise(
          addressDataFromBech32(contracts.fraudProof.spendingScriptAddress),
        ),
        AddressData,
      ),
    );
    const expectedStep02Cbor = applyParamsToScript(
      compiledScript(blueprint, INVALID_RANGE_FAULT_PROOF_TITLES.step02),
      [
        contracts.fraudProof.policyId,
        fraudProofTokenAddressData,
        contracts.computationThread.policyId,
      ],
    );
    const expectedStep01Cbor = applyParamsToScript(
      compiledScript(blueprint, INVALID_RANGE_FAULT_PROOF_TITLES.step01),
      [
        spendingScriptHash(expectedStep02Cbor),
        contracts.computationThread.policyId,
        h28b,
      ],
    );

    expect(contracts.invalidRange.steps[1].spendingScriptCBOR).toBe(
      expectedStep02Cbor,
    );
    expect(contracts.invalidRange.steps[1].spendingScriptHash).toBe(
      spendingScriptHash(expectedStep02Cbor),
    );
    expect(contracts.invalidRange.steps[0].spendingScriptCBOR).toBe(
      expectedStep01Cbor,
    );
    expect(contracts.invalidRange.steps[0].spendingScriptHash).toBe(
      spendingScriptHash(expectedStep01Cbor),
    );
    expect(contracts.invalidRange.steps[0].spendingScriptAddress).toBe(
      validatorToAddress("Preprod", spendingScript(expectedStep01Cbor)),
    );
  });

  it("builds invalid-range without requiring unrelated category validators", async () => {
    const blueprint = filterBlueprint(loadBlueprint(), [
      ...Object.values(FAULT_PROOF_SHARED_TITLES),
      ...Object.values(INVALID_RANGE_FAULT_PROOF_TITLES),
    ]);

    const contracts = await Effect.runPromise(
      buildInvalidRangeFaultProofContracts({
        blueprint,
        network: "Preprod",
        hubOraclePolicyId: h28b,
        fraudProofCataloguePolicyId: h28c,
      }),
    );

    expect(contracts.invalidRange.firstStep).toBe(
      contracts.invalidRange.steps[0],
    );
    expect(contracts.invalidRange.steps).toHaveLength(2);
  });

  it("builds zero-input with the validator parameter order from the blueprint", async () => {
    const blueprint = loadBlueprint();

    const contracts = await Effect.runPromise(
      buildZeroInputFaultProofContracts({
        blueprint,
        network: "Preprod",
        hubOraclePolicyId: h28b,
        fraudProofCataloguePolicyId: h28c,
      }),
    );

    expect(contracts.zeroInput.firstStep).toBe(contracts.zeroInput.steps[0]);
    expect(contracts.zeroInput.steps).toHaveLength(2);
    expect(
      new Set(contracts.zeroInput.steps.map((step) => step.spendingScriptHash))
        .size,
    ).toBe(2);

    const fraudProofTokenAddressData = Data.from(
      Data.to(
        await Effect.runPromise(
          addressDataFromBech32(contracts.fraudProof.spendingScriptAddress),
        ),
        AddressData,
      ),
    );
    const expectedStep02Cbor = applyParamsToScript(
      compiledScript(blueprint, ZERO_INPUT_FAULT_PROOF_TITLES.step02),
      [
        contracts.fraudProof.policyId,
        fraudProofTokenAddressData,
        contracts.computationThread.policyId,
        certificatePolicyId(blueprint),
      ],
    );
    const expectedStep01Cbor = applyParamsToScript(
      compiledScript(blueprint, ZERO_INPUT_FAULT_PROOF_TITLES.step01),
      [
        spendingScriptHash(expectedStep02Cbor),
        contracts.computationThread.policyId,
        h28b,
      ],
    );

    expect(contracts.zeroInput.steps[1].spendingScriptCBOR).toBe(
      expectedStep02Cbor,
    );
    expect(contracts.zeroInput.steps[0].spendingScriptCBOR).toBe(
      expectedStep01Cbor,
    );
    expect(contracts.zeroInput.steps[0].spendingScriptAddress).toBe(
      validatorToAddress("Preprod", spendingScript(expectedStep01Cbor)),
    );
  });

  it("builds zero-input without requiring unrelated category validators", async () => {
    const blueprint = filterBlueprint(loadBlueprint(), [
      ...Object.values(FAULT_PROOF_SHARED_TITLES),
      ...Object.values(ZERO_INPUT_FAULT_PROOF_TITLES),
    ]);

    const contracts = await Effect.runPromise(
      buildZeroInputFaultProofContracts({
        blueprint,
        network: "Preprod",
        hubOraclePolicyId: h28b,
        fraudProofCataloguePolicyId: h28c,
      }),
    );

    expect(contracts.zeroInput.firstStep).toBe(contracts.zeroInput.steps[0]);
    expect(contracts.zeroInput.steps).toHaveLength(2);
  });

  it("builds invalid-signature with the validator parameter order from the blueprint", async () => {
    const blueprint = loadBlueprint();

    const contracts = await Effect.runPromise(
      SDK.buildInvalidSignatureFaultProofContracts({
        blueprint,
        network: "Preprod",
        hubOraclePolicyId: h28b,
        fraudProofCataloguePolicyId: h28c,
      }),
    );

    expect(contracts.invalidSignature.firstStep).toBe(
      contracts.invalidSignature.steps[0],
    );
    expect(contracts.invalidSignature.steps).toHaveLength(2);
    expect(
      new Set(
        contracts.invalidSignature.steps.map((step) => step.spendingScriptHash),
      ).size,
    ).toBe(2);

    const fraudProofTokenAddressData = Data.from(
      Data.to(
        await Effect.runPromise(
          addressDataFromBech32(contracts.fraudProof.spendingScriptAddress),
        ),
        AddressData,
      ),
    );
    // Note the parameter order differs from zero-input/invalid-range: this
    // chain's final step takes the computation-thread policy first, matching
    // the aiken `validator main(...)` signature.
    const expectedStep02Cbor = applyParamsToScript(
      compiledScript(
        blueprint,
        SDK.INVALID_SIGNATURE_FAULT_PROOF_TITLES.step02,
      ),
      [
        contracts.computationThread.policyId,
        contracts.fraudProof.policyId,
        fraudProofTokenAddressData,
        certificatePolicyId(blueprint),
      ],
    );
    const expectedStep01Cbor = applyParamsToScript(
      compiledScript(
        blueprint,
        SDK.INVALID_SIGNATURE_FAULT_PROOF_TITLES.step01,
      ),
      [
        spendingScriptHash(expectedStep02Cbor),
        contracts.computationThread.policyId,
        h28b,
      ],
    );

    expect(contracts.invalidSignature.steps[1].spendingScriptCBOR).toBe(
      expectedStep02Cbor,
    );
    expect(contracts.invalidSignature.steps[0].spendingScriptCBOR).toBe(
      expectedStep01Cbor,
    );
    expect(contracts.invalidSignature.steps[0].spendingScriptAddress).toBe(
      validatorToAddress("Preprod", spendingScript(expectedStep01Cbor)),
    );
  });

  it("builds invalid-signature without requiring unrelated category validators", async () => {
    const blueprint = filterBlueprint(loadBlueprint(), [
      ...Object.values(FAULT_PROOF_SHARED_TITLES),
      ...Object.values(SDK.INVALID_SIGNATURE_FAULT_PROOF_TITLES),
    ]);

    const contracts = await Effect.runPromise(
      SDK.buildInvalidSignatureFaultProofContracts({
        blueprint,
        network: "Preprod",
        hubOraclePolicyId: h28b,
        fraudProofCataloguePolicyId: h28c,
      }),
    );

    expect(contracts.invalidSignature.firstStep).toBe(
      contracts.invalidSignature.steps[0],
    );
    expect(contracts.invalidSignature.steps).toHaveLength(2);
  });

  it("builds reference-input-no-idx with the validator parameter order from the blueprint", async () => {
    const blueprint = loadBlueprint();

    const contracts = await Effect.runPromise(
      SDK.buildReferenceInputNoIdxFaultProofContracts({
        blueprint,
        network: "Preprod",
        hubOraclePolicyId: h28b,
        fraudProofCataloguePolicyId: h28c,
      }),
    );

    expect(contracts.referenceInputNoIdx.firstStep).toBe(
      contracts.referenceInputNoIdx.steps[0],
    );
    expect(contracts.referenceInputNoIdx.steps).toHaveLength(4);
    expect(
      new Set(
        contracts.referenceInputNoIdx.steps.map(
          (step) => step.spendingScriptHash,
        ),
      ).size,
    ).toBe(4);

    const fraudProofTokenAddressData = Data.from(
      Data.to(
        await Effect.runPromise(
          addressDataFromBech32(contracts.fraudProof.spendingScriptAddress),
        ),
        AddressData,
      ),
    );
    // Same applied-parameter order as input-no-idx, taken from the blueprint.
    const expectedStep04Cbor = applyParamsToScript(
      compiledScript(
        blueprint,
        SDK.REFERENCE_INPUT_NO_IDX_FAULT_PROOF_TITLES.step04,
      ),
      [
        contracts.computationThread.policyId,
        contracts.fraudProof.policyId,
        fraudProofTokenAddressData,
        certificatePolicyId(blueprint),
      ],
    );
    const expectedStep03Cbor = applyParamsToScript(
      compiledScript(
        blueprint,
        SDK.REFERENCE_INPUT_NO_IDX_FAULT_PROOF_TITLES.step03,
      ),
      [
        spendingScriptHash(expectedStep04Cbor),
        contracts.computationThread.policyId,
        h28b,
      ],
    );
    const expectedStep02Cbor = applyParamsToScript(
      compiledScript(
        blueprint,
        SDK.REFERENCE_INPUT_NO_IDX_FAULT_PROOF_TITLES.step02,
      ),
      [
        spendingScriptHash(expectedStep03Cbor),
        contracts.computationThread.policyId,
        certificatePolicyId(blueprint),
      ],
    );
    const expectedStep01Cbor = applyParamsToScript(
      compiledScript(
        blueprint,
        SDK.REFERENCE_INPUT_NO_IDX_FAULT_PROOF_TITLES.step01,
      ),
      [
        spendingScriptHash(expectedStep02Cbor),
        contracts.computationThread.policyId,
        h28b,
      ],
    );

    expect(contracts.referenceInputNoIdx.steps[3].spendingScriptCBOR).toBe(
      expectedStep04Cbor,
    );
    expect(contracts.referenceInputNoIdx.steps[2].spendingScriptCBOR).toBe(
      expectedStep03Cbor,
    );
    expect(contracts.referenceInputNoIdx.steps[1].spendingScriptCBOR).toBe(
      expectedStep02Cbor,
    );
    expect(contracts.referenceInputNoIdx.steps[0].spendingScriptCBOR).toBe(
      expectedStep01Cbor,
    );
    expect(contracts.referenceInputNoIdx.steps[0].spendingScriptHash).toBe(
      spendingScriptHash(expectedStep01Cbor),
    );
    expect(contracts.referenceInputNoIdx.steps[0].spendingScriptAddress).toBe(
      validatorToAddress("Preprod", spendingScript(expectedStep01Cbor)),
    );
  });

  it("builds reference-input-no-idx without requiring unrelated category validators", async () => {
    const blueprint = filterBlueprint(loadBlueprint(), [
      ...Object.values(FAULT_PROOF_SHARED_TITLES),
      ...Object.values(SDK.REFERENCE_INPUT_NO_IDX_FAULT_PROOF_TITLES),
    ]);

    const contracts = await Effect.runPromise(
      SDK.buildReferenceInputNoIdxFaultProofContracts({
        blueprint,
        network: "Preprod",
        hubOraclePolicyId: h28b,
        fraudProofCataloguePolicyId: h28c,
      }),
    );

    expect(contracts.referenceInputNoIdx.firstStep).toBe(
      contracts.referenceInputNoIdx.steps[0],
    );
    expect(contracts.referenceInputNoIdx.steps).toHaveLength(4);
  });

  it("builds transition-trace with the validator parameter order from the blueprint", async () => {
    const blueprint = loadBlueprint();

    const contracts = await Effect.runPromise(
      buildTransitionTraceFaultProofContracts({
        eventHistoryBounds: {
          inlineLimitBytes: 512n,
          maxPayloadBytes: 5000n,
          maxPayloadNodes: 512n,
        },
        blueprint,
        network: "Preprod",
        hubOraclePolicyId: h28b,
        fraudProofCataloguePolicyId: h28c,
        referenceScriptAuthPolicyId: h28b,
      }),
    );

    expect(contracts.transitionTrace.firstStep).toBe(
      contracts.transitionTrace.steps[0],
    );
    expect(contracts.transitionTrace.steps).toHaveLength(9);

    const fraudProofTokenAddressData = Data.from(
      Data.to(
        await Effect.runPromise(
          addressDataFromBech32(contracts.fraudProof.spendingScriptAddress),
        ),
        AddressData,
      ),
    );
    const historyRetentionData = await Promise.all(
      [
        contracts.transitionTrace.history.retentionAddresses.deposit,
        contracts.transitionTrace.history.retentionAddresses.withdrawal,
      ].map(async (address) =>
        Data.from(
          Data.to(
            await Effect.runPromise(addressDataFromBech32(address)),
            AddressData,
          ),
        ),
      ),
    );
    const finalNames = [
      "control",
      "source",
      "withdrawal",
      "forced",
      "accepted",
      "deposit",
      "l1Event",
      "duplicate",
    ] as const;
    const expectedFinalCbors = finalNames.map((name) =>
      applyParamsToScript(
        compiledScript(blueprint, TRANSITION_TRACE_FAULT_PROOF_TITLES[name]),
        [
          contracts.computationThread.policyId,
          contracts.fraudProof.policyId,
          fraudProofTokenAddressData,
          ...(name === "accepted" || name === "deposit" ? [h28b] : []),
          ...(name === "deposit" ? [h28b] : []),
          ...(name === "l1Event" ? [h28b, h28b] : []),
        ],
      ),
    );
    expect(
      contracts.transitionTrace.finals.map(
        ({ spendingScriptCBOR }) => spendingScriptCBOR,
      ),
    ).toEqual(expectedFinalCbors);
    expect(
      contracts.transitionTrace.yields.l1Event.withdrawalScript.script,
    ).toEqual(
      applyParamsToScript(
        compiledScript(blueprint, SDK.TRANSITION_TRACE_YIELD_TITLES.l1Event),
        [
          contracts.transitionTrace.finals[6].spendingScriptHash,
          contracts.computationThread.policyId,
          h28b,
          ...historyRetentionData,
          512n,
          5000n,
          512n,
        ],
      ),
    );
    expect(
      contracts.transitionTrace.yields.forcedTiming.withdrawalScript.script,
    ).toEqual(
      applyParamsToScript(
        compiledScript(
          blueprint,
          SDK.TRANSITION_TRACE_YIELD_TITLES.forcedTiming,
        ),
        [
          contracts.transitionTrace.finals[6].spendingScriptHash,
          contracts.computationThread.policyId,
          h28b,
        ],
      ),
    );
    const finalHashesSchema = Data.Array(Data.Bytes());
    type FinalHashes = Data.Static<typeof finalHashesSchema>;
    const FinalHashes = asDataType<FinalHashes>(finalHashesSchema);
    const finalHashesData = Data.from(
      Data.to(
        expectedFinalCbors.map((cbor) => spendingScriptHash(cbor)),
        FinalHashes,
      ),
    );
    const expectedRouteCbor = applyParamsToScript(
      compiledScript(blueprint, TRANSITION_TRACE_FAULT_PROOF_TITLES.route),
      [finalHashesData, contracts.computationThread.policyId],
    );
    expect(contracts.transitionTrace.route.spendingScriptCBOR).toBe(
      expectedRouteCbor,
    );
    expect(contracts.transitionTrace.route.spendingScriptAddress).toBe(
      validatorToAddress("Preprod", spendingScript(expectedRouteCbor)),
    );
  });

  it("builds validation-trace dispute with its exact shared-policy parameter order", async () => {
    const rawBlueprint = JSON.parse(readFileSync(blueprintPath, "utf8")) as {
      readonly validators?: readonly {
        readonly title?: string;
        readonly parameters?: readonly unknown[];
      }[];
    };
    expect(
      rawBlueprint.validators?.find(
        ({ title }) =>
          title === VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.prepares.cek,
      )?.parameters,
    ).toHaveLength(2);
    const blueprint = filterBlueprint(loadBlueprint(), [
      // Derived from the production title constants rather than transcribed,
      // so a stage added to the family cannot silently fall out of the
      // allowlist. Only this family's titles (plus the shared ones) are
      // admitted, so the leg still shows the builder needs no other family.
      ...collectTitles(FAULT_PROOF_SHARED_TITLES),
      ...collectTitles(VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES),
      ...collectTitles(CEK_CORE_STAGE_TITLES),
      ...collectTitles(CEK_CONTEXT_STAGE_TITLES),
      ...CEK_REDEEMER_ITEM_TITLES,
      ...CEK_MATERIAL_TRAVERSAL_TITLES,
      CEK_PROGRAM_MATERIAL_SPEND_TITLE,
    ]);

    const contracts = await Effect.runPromise(
      buildValidationTraceDisputeFaultProofContracts({
        blueprint,
        network: "Preprod",
        hubOraclePolicyId: h28b,
        fraudProofCataloguePolicyId: h28c,
        referenceScriptAuthPolicyId: h28,
      }),
    );
    const fraudProofTokenAddressData = Data.from(
      Data.to(
        await Effect.runPromise(
          addressDataFromBech32(contracts.fraudProof.spendingScriptAddress),
        ),
        AddressData,
      ),
    );
    const expectedAward = applyParamsToScript(
      compiledScript(
        blueprint,
        VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.award,
      ),
      [
        contracts.computationThread.policyId,
        contracts.fraudProof.policyId,
        fraudProofTokenAddressData,
      ],
    );
    const deploymentId = deriveValidationTraceDeploymentId(h28c);
    const expectedStageOneRedeemerFoldMapExecutor = applyParamsToScript(
      compiledScript(
        blueprint,
        VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES
          .scriptSourcesStageOneRedeemerStages.foldMapExecutor,
      ),
      [deploymentId, contracts.computationThread.policyId],
    );
    const expectedStageOneRedeemerFinalizeFrameExecutor = applyParamsToScript(
      compiledScript(
        blueprint,
        VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES
          .scriptSourcesStageOneRedeemerStages.finalizeFrameExecutor,
      ),
      [deploymentId, contracts.computationThread.policyId],
    );
    const sharedTitles =
      VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.scriptSourcesStageOneRedeemerStages;
    const expectedStageOneRedeemerSourceAuthenticator = applyParamsToScript(
      compiledScript(blueprint, sharedTitles.sourceAuthenticator),
      [deploymentId, contracts.computationThread.policyId],
    );
    const expectedExecutorTitles = [
      sharedTitles.foldMapExecutor,
      sharedTitles.finalizeFrameExecutor,
      sharedTitles.openHeaderExecutor,
      sharedTitles.openTailExecutor,
      sharedTitles.headScalarExecutor,
      sharedTitles.headSequenceExecutor,
      sharedTitles.headMapExecutor,
      sharedTitles.headLargeConstructorExecutor,
      sharedTitles.attachIntegerExecutor,
      sharedTitles.attachBytesExecutor,
      sharedTitles.foldListExecutor,
      sharedTitles.advanceIntegerExecutor,
      sharedTitles.advanceBytesExecutor,
      sharedTitles.advanceLargeConstructorExecutor,
      sharedTitles.advanceLargeFieldsExecutor,
      sharedTitles.closeExecutor,
      sharedTitles.finishDataExecutor,
      sharedTitles.invalidHeaderExecutor,
      sharedTitles.invalidTailExecutor,
    ];
    const expectedExecutors = expectedExecutorTitles.map((title) =>
      applyParamsToScript(compiledScript(blueprint, title), [
        deploymentId,
        contracts.computationThread.policyId,
      ]),
    );
    const expectedExecutorHashes = expectedExecutors.map(spendingScriptHash);
    const expectedStageOneRedeemerOuterNormalizer = applyParamsToScript(
      compiledScript(
        blueprint,
        VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES
          .scriptSourcesStageOneRedeemerStages.outerNormalizer,
      ),
      [
        deploymentId,
        contracts.computationThread.policyId,
        spendingScriptHash(expectedStageOneRedeemerSourceAuthenticator),
      ],
    );
    const expectedStageOneRedeemerTraversalNormalizer = applyParamsToScript(
      compiledScript(
        blueprint,
        VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES
          .scriptSourcesStageOneRedeemerStages.traversalNormalizer,
      ),
      [deploymentId, contracts.computationThread.policyId],
    );
    const expectedStageOneRedeemerSettlement = applyParamsToScript(
      compiledScript(
        blueprint,
        VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES
          .scriptSourcesStageOneRedeemerStages.settlement,
      ),
      [
        deploymentId,
        spendingScriptHash(expectedStageOneRedeemerTraversalNormalizer),
        spendingScriptHash(expectedStageOneRedeemerOuterNormalizer),
        expectedExecutorHashes,
        spendingScriptHash(expectedAward),
        contracts.computationThread.policyId,
      ],
    );
    const expectedStageOneRedeemerEnvelope = applyParamsToScript(
      compiledScript(
        blueprint,
        VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES
          .scriptSourcesStageOneRedeemerStages.envelope,
      ),
      [
        deploymentId,
        spendingScriptHash(expectedStageOneRedeemerTraversalNormalizer),
        spendingScriptHash(expectedStageOneRedeemerOuterNormalizer),
        expectedExecutorHashes,
        spendingScriptHash(expectedStageOneRedeemerSettlement),
        contracts.computationThread.policyId,
      ],
    );
    // The blueprint's own declared parameter list is the oracle for *which*
    // argument goes in *which* position: this table maps each declared
    // parameter title to the one value the deployment may bind to it, and the
    // expectation is assembled by reading the titles off the blueprint entry.
    // An argument order that disagrees with the blueprint therefore fails, and
    // a parameter added to a resolver without a reviewed binding here fails
    // closed rather than silently defaulting.
    const dispute = contracts.validationTraceDispute;
    const semanticParameterBindings: Readonly<Record<string, Data>> = {
      award_script_hash: spendingScriptHash(expectedAward),
      computation_thread_policy_id: contracts.computationThread.policyId,
      field_preimage_certificate_policy_id: certificatePolicyId(blueprint),
      reference_script_auth_policy_id: h28,
      source_binder_script_hash:
        dispute.canonicalDecodeItemStages.source.spendingScriptHash,
      cek_program_material_script_hash:
        dispute.cekProgramMaterial.spendingScriptHash,
      cek_material_traversal_script_hash:
        dispute.cekMaterialTraversal.spendingScriptHash,
      cek_context_control_script_hash:
        dispute.cekContextStages.control.spendingScriptHash,
      arm_script_hashes: cekCoreEntryHashes(dispute.cekCoreStages),
    };
    const expectedBaseSemanticResolvers = Object.values(
      VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.semantics,
    ).map((title) => {
      const declared = blueprint.validators.find(
        (entry) => entry.title === title,
      )?.parameters;
      if (declared === undefined) {
        throw new Error(`Missing declared parameters for ${title}`);
      }
      return applyParamsToScript(
        compiledScript(blueprint, title),
        declared.map(({ title: parameterTitle }) => {
          const bound = semanticParameterBindings[parameterTitle];
          if (bound === undefined || bound === null) {
            throw new Error(
              `Unreviewed semantic resolver parameter ${parameterTitle} on ${title}`,
            );
          }
          return bound;
        }),
      );
    });
    const expectedSemanticResolvers = [
      ...expectedBaseSemanticResolvers,
      expectedStageOneRedeemerEnvelope,
    ];
    const expectedSemanticResolverGroups = [
      [expectedSemanticResolvers[0]!, expectedSemanticResolvers[1]!],
      [expectedSemanticResolvers[2]!],
      [expectedSemanticResolvers[3]!],
      [expectedSemanticResolvers[4]!, expectedSemanticResolvers[5]!],
      [
        expectedSemanticResolvers[6]!,
        expectedSemanticResolvers[7]!,
        expectedSemanticResolvers[8]!,
        expectedSemanticResolvers[9]!,
      ],
      [
        expectedSemanticResolvers[10]!,
        expectedSemanticResolvers[11]!,
        expectedSemanticResolvers[12]!,
        expectedSemanticResolvers[13]!,
        expectedSemanticResolvers[14]!,
        expectedSemanticResolvers[15]!,
        expectedSemanticResolvers[16]!,
        expectedSemanticResolvers[17]!,
        expectedSemanticResolvers[18]!,
        expectedSemanticResolvers[19]!,
        expectedSemanticResolvers[20]!,
        expectedSemanticResolvers[21]!,
        expectedSemanticResolvers[22]!,
        expectedSemanticResolvers[23]!,
      ],
      [expectedSemanticResolvers[24]!, expectedSemanticResolvers[25]!],
      [
        expectedSemanticResolvers[26]!,
        expectedSemanticResolvers[27]!,
        expectedSemanticResolvers[28]!,
        expectedSemanticResolvers[29]!,
        expectedSemanticResolvers[30]!,
        expectedSemanticResolvers[31]!,
      ],
      [
        expectedSemanticResolvers[32]!,
        expectedSemanticResolvers[33]!,
        expectedSemanticResolvers[34]!,
        expectedSemanticResolvers[35]!,
        expectedSemanticResolvers[36]!,
        expectedSemanticResolvers[37]!,
        expectedSemanticResolvers[38]!,
        expectedSemanticResolvers[39]!,
        expectedSemanticResolvers[40]!,
        expectedSemanticResolvers[41]!,
        expectedSemanticResolvers[42]!,
        expectedSemanticResolvers[43]!,
        expectedSemanticResolvers[44]!,
        expectedSemanticResolvers[45]!,
        expectedSemanticResolvers[46]!,
        expectedSemanticResolvers[47]!,
        expectedSemanticResolvers[48]!,
        expectedSemanticResolvers[49]!,
        expectedSemanticResolvers[50]!,
        expectedSemanticResolvers[51]!,
        expectedSemanticResolvers[52]!,
        expectedSemanticResolvers[53]!,
        expectedSemanticResolvers[54]!,
        expectedSemanticResolvers[55]!,
        expectedSemanticResolvers[56]!,
        expectedSemanticResolvers[57]!,
        expectedSemanticResolvers[58]!,
        expectedSemanticResolvers[59]!,
        expectedSemanticResolvers[90]!,
      ],
      [
        expectedSemanticResolvers[60]!,
        expectedSemanticResolvers[61]!,
        expectedSemanticResolvers[62]!,
      ],
      [
        expectedSemanticResolvers[63]!,
        expectedSemanticResolvers[64]!,
        expectedSemanticResolvers[65]!,
        expectedSemanticResolvers[66]!,
      ],
      [
        expectedSemanticResolvers[67]!,
        expectedSemanticResolvers[68]!,
        expectedSemanticResolvers[69]!,
        expectedSemanticResolvers[70]!,
      ],
      [
        expectedSemanticResolvers[71]!,
        expectedSemanticResolvers[72]!,
        expectedSemanticResolvers[73]!,
        expectedSemanticResolvers[74]!,
        expectedSemanticResolvers[75]!,
        expectedSemanticResolvers[76]!,
        expectedSemanticResolvers[77]!,
        expectedSemanticResolvers[78]!,
        expectedSemanticResolvers[79]!,
        expectedSemanticResolvers[80]!,
        expectedSemanticResolvers[81]!,
      ],
      [
        expectedSemanticResolvers[82]!,
        expectedSemanticResolvers[83]!,
        expectedSemanticResolvers[84]!,
        expectedSemanticResolvers[85]!,
        expectedSemanticResolvers[86]!,
        expectedSemanticResolvers[87]!,
        expectedSemanticResolvers[88]!,
        expectedSemanticResolvers[89]!,
      ],
    ] as const;
    const resolverHashesSchema = Data.Array(Data.Bytes());
    type ResolverHashes = Data.Static<typeof resolverHashesSchema>;
    const ResolverHashes = asDataType<ResolverHashes>(resolverHashesSchema);
    const expectedSemanticResolverHashParams =
      expectedSemanticResolverGroups.map(
        (group) =>
          Data.from(
            Data.to(group.map(spendingScriptHash), ResolverHashes),
          ) as Data,
      );
    const expectedPrepareResolvers = Object.values(
      VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.prepares,
    ).map((title, index) =>
      applyParamsToScript(compiledScript(blueprint, title), [
        expectedSemanticResolverHashParams[index]!,
        contracts.computationThread.policyId,
      ]),
    );
    const expectedCekProgramMaterial = compiledScript(
      blueprint,
      CEK_PROGRAM_MATERIAL_SPEND_TITLE,
    );
    const expectedResolvers = [
      expectedPrepareResolvers[0]!,
      expectedPrepareResolvers[1]!,
      expectedPrepareResolvers[2]!,
      expectedPrepareResolvers[3]!,
      expectedPrepareResolvers[4]!,
      expectedPrepareResolvers[5]!,
      expectedPrepareResolvers[6]!,
      expectedPrepareResolvers[7]!,
      expectedPrepareResolvers[8]!,
      expectedPrepareResolvers[9]!,
      expectedPrepareResolvers[10]!,
      expectedPrepareResolvers[11]!,
      expectedPrepareResolvers[12]!,
      expectedPrepareResolvers[13]!,
    ];
    expect(expectedResolvers).toHaveLength(VALIDATION_TRACE_RESOLVER_COUNT);
    const resolverHashesData = Data.from(
      Data.to(
        expectedResolvers.map((cbor) => spendingScriptHash(cbor)),
        ResolverHashes,
      ),
    );
    const expectedBoundaryCbor = applyParamsToScript(
      compiledScript(
        blueprint,
        VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.boundary,
      ),
      [resolverHashesData, contracts.computationThread.policyId],
    );
    const expectedTimeoutCbor = applyParamsToScript(
      compiledScript(
        blueprint,
        VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.timeout,
      ),
      [
        contracts.computationThread.policyId,
        contracts.fraudProof.policyId,
        fraudProofTokenAddressData,
      ],
    );
    const expectedGameCbor = applyParamsToScript(
      compiledScript(
        blueprint,
        VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.game,
      ),
      [
        spendingScriptHash(expectedBoundaryCbor),
        spendingScriptHash(expectedTimeoutCbor),
        contracts.computationThread.policyId,
      ],
    );
    const expectedSourceCbor = applyParamsToScript(
      compiledScript(
        blueprint,
        VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.source,
      ),
      [
        spendingScriptHash(expectedGameCbor),
        spendingScriptHash(expectedAward),
        contracts.computationThread.policyId,
      ],
    );
    const expectedCbor = applyParamsToScript(
      compiledScript(
        blueprint,
        VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.dispute,
      ),
      [
        spendingScriptHash(expectedSourceCbor),
        contracts.computationThread.policyId,
        h28b,
      ],
    );
    for (const [label, cbor] of [
      ["opener", expectedCbor],
      ["source", expectedSourceCbor],
      ["game", expectedGameCbor],
      ["boundary", expectedBoundaryCbor],
      ["timeout", expectedTimeoutCbor],
      ["award", expectedAward],
      ...expectedPrepareResolvers.map(
        (cbor, prepareIndex) =>
          [`prepare-${prepareIndex.toString()}`, cbor] as const,
      ),
    ] as const) {
      expect(
        cbor.length / 2,
        `${label} parameterized script bytes`,
      ).toBeLessThanOrEqual(MAX_APPLIED_SCRIPT_BYTES);
    }

    expect(contracts.validationTraceDispute.steps).toHaveLength(
      VALIDATION_TRACE_DISPUTE_STEP_COUNT,
    );
    expect(contracts.validationTraceDispute.award.spendingScriptCBOR).toBe(
      expectedAward,
    );
    expect(
      contracts.validationTraceDispute.semanticResolvers.map(
        ({ spendingScriptCBOR }) => spendingScriptCBOR,
      ),
    ).toEqual(expectedSemanticResolvers);
    expect(
      contracts.validationTraceDispute.scriptSourcesStageOneRedeemerStages.executors.map(
        ({ spendingScriptCBOR }) => spendingScriptCBOR,
      ),
    ).toEqual(expectedExecutors);
    const actualSharedStages =
      contracts.validationTraceDispute.scriptSourcesStageOneRedeemerStages;
    expect(
      [
        actualSharedStages.envelope,
        actualSharedStages.traversalNormalizer,
        actualSharedStages.outerNormalizer,
        actualSharedStages.sourceAuthenticator,
        actualSharedStages.foldMapExecutor,
        actualSharedStages.finalizeFrameExecutor,
        actualSharedStages.settlement,
      ].map(({ spendingScriptCBOR }) => spendingScriptCBOR),
    ).toEqual([
      expectedStageOneRedeemerEnvelope,
      expectedStageOneRedeemerTraversalNormalizer,
      expectedStageOneRedeemerOuterNormalizer,
      expectedStageOneRedeemerSourceAuthenticator,
      expectedStageOneRedeemerFoldMapExecutor,
      expectedStageOneRedeemerFinalizeFrameExecutor,
      expectedStageOneRedeemerSettlement,
    ]);
    expect(
      contracts.validationTraceDispute.prepareResolvers.map(
        ({ spendingScriptCBOR }) => spendingScriptCBOR,
      ),
    ).toEqual(expectedPrepareResolvers);
    expect(
      contracts.validationTraceDispute.cekProgramMaterial.spendingScriptCBOR,
    ).toBe(expectedCekProgramMaterial);
    expect(
      contracts.validationTraceDispute.resolvers.map(
        ({ spendingScriptCBOR }) => spendingScriptCBOR,
      ),
    ).toEqual(expectedResolvers);
    expect(contracts.validationTraceDispute.boundary.spendingScriptCBOR).toBe(
      expectedBoundaryCbor,
    );
    expect(contracts.validationTraceDispute.timeout.spendingScriptCBOR).toBe(
      expectedTimeoutCbor,
    );
    expect(contracts.validationTraceDispute.game.spendingScriptCBOR).toBe(
      expectedGameCbor,
    );
    expect(contracts.validationTraceDispute.source.spendingScriptCBOR).toBe(
      expectedSourceCbor,
    );
    expect(contracts.validationTraceDispute.firstStep.spendingScriptCBOR).toBe(
      expectedCbor,
    );
    expect(contracts.validationTraceDispute.firstStep.spendingScriptHash).toBe(
      spendingScriptHash(expectedCbor),
    );
    expect(
      contracts.validationTraceDispute.firstStep.spendingScriptAddress,
    ).toBe(validatorToAddress("Preprod", spendingScript(expectedCbor)));
  });
});
