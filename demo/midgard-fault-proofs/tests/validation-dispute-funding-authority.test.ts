/** The validation-trace dispute's funding terms and VerifySource authority
 * outside their category: a runtime funding policy refuses the terms for any
 * other category, and a funding permit refuses the authority on any other
 * category's workflow. Within it, the policy binds the thread moves'
 * contracts only when each is a rostered proof-thread contract of its own,
 * with the compiled profile's per-move fee cap and receipt lovelace, and
 * the one-step holders a thread visits likewise. The emulator suite covers
 * the authorities' own category. */
import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import { CML, credentialToAddress } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { grantValidationDisputeFundingAuthority } from "../src/workflow/funding-reservation-permit.assert-validation-dispute-funding.js";
import { createAdmittedWorkflowRunner } from "../src/workflow/runner-admission.js";
import { validationDisputeWalletMoveReserve } from "../src/workflow/runtime-funding-fee.js";
import {
  createWorkflowRuntimeFundingPolicy,
  readWorkflowRuntimeFundingPolicy,
} from "../src/workflow/runtime-funding-policy.js";
import { bindWorkflowPreflightTransaction } from "../src/workflow/transaction-boundary.js";
import { runtimeFundingPolicyFixture } from "./helpers/runtime-funding-policy-fixture.js";
import {
  DEPLOYMENT,
  fundingAddress,
} from "./workflow-runtime.admitted-actuation.js";
import {
  runtimeFunding,
  signedFundingTransaction,
} from "./workflow-runtime.runtime-funding.js";

const DEPLOYMENT_FINGERPRINT = "11".repeat(32);
const scriptAddress = (hash: string) =>
  credentialToAddress("Preprod", { type: "Script", hash });
const BOND_ADDRESS = scriptAddress("b5".repeat(28));
const CASE_ADDRESS = scriptAddress("c5".repeat(28));
const GAME_ADDRESS = scriptAddress("6a".repeat(28));
const TIMEOUT_ADDRESS = scriptAddress("7a".repeat(28));
const AWARD_ADDRESS = scriptAddress("8a".repeat(28));
const BOUNDARY_ADDRESS = scriptAddress("0b".repeat(28));
const RESOLVER_ADDRESSES = [
  scriptAddress("1c".repeat(28)),
  scriptAddress("2c".repeat(28)),
];
const ONE_STEP = {
  boundaryAddress: BOUNDARY_ADDRESS,
  resolverAddresses: RESOLVER_ADDRESSES,
};
const MOVES = {
  gameAddress: GAME_ADDRESS,
  timeoutAddress: TIMEOUT_ADDRESS,
  awardAddress: AWARD_ADDRESS,
  awardPolicyId: "9a".repeat(28),
};
const proofThread = (address: string, scriptHash: string) => ({
  address,
  scriptHash,
  role: "proof_thread" as const,
});

const disputePolicyInput = () => {
  const { constructorInput } = runtimeFundingPolicyFixture({
    deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
    fundingPaymentKeyHash: CML.PrivateKey.from_normal_bytes(
      Buffer.alloc(32, 0x23),
    )
      .to_public()
      .hash()
      .to_hex(),
    contracts: [
      proofThread(BOND_ADDRESS, "b5".repeat(28)),
      proofThread(CASE_ADDRESS, "c5".repeat(28)),
      proofThread(GAME_ADDRESS, "6a".repeat(28)),
      proofThread(TIMEOUT_ADDRESS, "7a".repeat(28)),
      proofThread(AWARD_ADDRESS, "8a".repeat(28)),
      proofThread(BOUNDARY_ADDRESS, "0b".repeat(28)),
      proofThread(RESOLVER_ADDRESSES[0]!, "1c".repeat(28)),
      proofThread(RESOLVER_ADDRESSES[1]!, "2c".repeat(28)),
    ],
  });
  return {
    constructorInput,
    terms: {
      deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
      bondLovelace: 10_000_000_000n,
      bondAddress: BOND_ADDRESS,
      caseAddress: CASE_ADDRESS,
      casePolicyId: "d5".repeat(28),
    },
  };
};

const disputeRunner = createAdmittedWorkflowRunner({
  category: "validationTraceDispute",
  runOrResume: async () => {
    throw new Error("the policy fixture runner is never executed");
  },
});

describe("validation-trace dispute funding outside its category", () => {
  it("binds the dispute terms only into a validation-trace dispute policy", () => {
    const { constructorInput, terms } = disputePolicyInput();
    expect(() =>
      createWorkflowRuntimeFundingPolicy({
        ...constructorInput,
        validationDispute: terms,
      }),
    ).toThrow(
      "runtime funding validation dispute terms are refused for category doubleSpend",
    );
    const policy = readWorkflowRuntimeFundingPolicy(
      createWorkflowRuntimeFundingPolicy({
        ...constructorInput,
        category: "validationTraceDispute",
        runner: disputeRunner,
        validationDispute: terms,
      }),
    );
    expect(policy.validationDispute).toStrictEqual({
      bondLovelace: "10000000000",
      caseFloorLovelace: "3200000",
      bondAddress: BOND_ADDRESS,
      caseAddress: CASE_ADDRESS,
      casePolicyId: terms.casePolicyId,
    });
  });

  it("binds the thread moves' contracts with the compiled per-move fee cap, receipt lovelace and the wallet-move reserve at the policy's protocol parameters", () => {
    const { constructorInput, terms } = disputePolicyInput();
    const policy = readWorkflowRuntimeFundingPolicy(
      createWorkflowRuntimeFundingPolicy({
        ...constructorInput,
        category: "validationTraceDispute",
        runner: disputeRunner,
        validationDispute: { ...terms, moves: MOVES },
      }),
    );
    const { limits } = SELECTED_DEPLOYMENT_PROFILE;
    expect(policy.validationDispute?.moves).toStrictEqual({
      ...MOVES,
      moveFeeCapLovelace: limits.max_dispute_move_fee_lovelace.toString(),
      fraudProofOutputLovelace: limits.fraud_proof_output_lovelace.toString(),
      walletMoveReserveLovelace: validationDisputeWalletMoveReserve({
        parameters: policy.protocolParameters,
        fundingPaymentKeyHash: constructorInput.fundingPaymentKeyHash,
      }).toString(),
    });
  });

  it("binds the one-step holders a thread visits between the game and the award", () => {
    const { constructorInput, terms } = disputePolicyInput();
    const policy = readWorkflowRuntimeFundingPolicy(
      createWorkflowRuntimeFundingPolicy({
        ...constructorInput,
        category: "validationTraceDispute",
        runner: disputeRunner,
        validationDispute: { ...terms, moves: { ...MOVES, oneStep: ONE_STEP } },
      }),
    );
    expect(policy.validationDispute?.moves?.oneStep).toStrictEqual(ONE_STEP);
  });

  it.each([
    {
      name: "no resolver",
      oneStep: { resolverAddresses: [] },
      refusal:
        "runtime funding validation dispute one-step terms name no resolver",
    },
    {
      name: "a boundary at an unrostered address",
      oneStep: { boundaryAddress: scriptAddress("e5".repeat(28)) },
      refusal:
        "runtime funding validation dispute boundary address is not a rostered proof-thread contract",
    },
    {
      name: "a resolver at an unrostered address",
      oneStep: {
        resolverAddresses: [
          RESOLVER_ADDRESSES[0]!,
          scriptAddress("e5".repeat(28)),
        ],
      },
      refusal:
        "runtime funding validation dispute one-step resolver address is not a rostered proof-thread contract",
    },
    {
      name: "a resolver at the boundary address",
      oneStep: { resolverAddresses: [BOUNDARY_ADDRESS] },
      refusal: "runtime funding validation dispute contract addresses coincide",
    },
    {
      name: "a boundary at the game address",
      oneStep: { boundaryAddress: GAME_ADDRESS },
      refusal: "runtime funding validation dispute contract addresses coincide",
    },
  ])("refuses one-step terms with $name", ({ oneStep, refusal }) => {
    const { constructorInput, terms } = disputePolicyInput();
    expect(() =>
      createWorkflowRuntimeFundingPolicy({
        ...constructorInput,
        category: "validationTraceDispute",
        runner: disputeRunner,
        validationDispute: {
          ...terms,
          moves: { ...MOVES, oneStep: { ...ONE_STEP, ...oneStep } },
        },
      }),
    ).toThrow(refusal);
  });

  it.each([
    {
      name: "a game at an unrostered address",
      moves: { gameAddress: scriptAddress("e5".repeat(28)) },
      refusal:
        "runtime funding validation dispute game address is not a rostered proof-thread contract",
    },
    {
      name: "a timeout at an unrostered address",
      moves: { timeoutAddress: scriptAddress("e5".repeat(28)) },
      refusal:
        "runtime funding validation dispute timeout address is not a rostered proof-thread contract",
    },
    {
      name: "an award at an unrostered address",
      moves: { awardAddress: scriptAddress("e5".repeat(28)) },
      refusal:
        "runtime funding validation dispute award address is not a rostered proof-thread contract",
    },
    {
      name: "a malformed award policy",
      moves: { awardPolicyId: "9a".repeat(27) },
      refusal: "runtime funding validation award policy is malformed",
    },
    {
      name: "a game at the case address",
      moves: { gameAddress: CASE_ADDRESS },
      refusal: "runtime funding validation dispute contract addresses coincide",
    },
    {
      name: "a timeout at the game address",
      moves: { timeoutAddress: GAME_ADDRESS },
      refusal: "runtime funding validation dispute contract addresses coincide",
    },
  ])("refuses thread-move terms with $name", ({ moves, refusal }) => {
    const { constructorInput, terms } = disputePolicyInput();
    expect(() =>
      createWorkflowRuntimeFundingPolicy({
        ...constructorInput,
        category: "validationTraceDispute",
        runner: disputeRunner,
        validationDispute: { ...terms, moves: { ...MOVES, ...moves } },
      }),
    ).toThrow(refusal);
  });

  it.each([
    {
      name: "another deployment's terms",
      amend: { deploymentFingerprint: "12".repeat(32) },
      refusal:
        "runtime funding validation dispute terms belong to another deployment",
    },
    {
      name: "a bond to an unrostered address",
      amend: { bondAddress: scriptAddress("e5".repeat(28)) },
      refusal:
        "runtime funding validation dispute bond address is not a rostered proof-thread contract",
    },
    {
      name: "a case at the bond address",
      amend: { caseAddress: BOND_ADDRESS },
      refusal: "runtime funding validation dispute contract addresses coincide",
    },
    {
      name: "no bond",
      amend: { bondLovelace: 0n },
      refusal:
        "runtime funding validation dispute bond is not a positive lovelace amount",
    },
  ])("refuses a dispute policy with $name", ({ amend, refusal }) => {
    const { constructorInput, terms } = disputePolicyInput();
    expect(() =>
      createWorkflowRuntimeFundingPolicy({
        ...constructorInput,
        category: "validationTraceDispute",
        runner: disputeRunner,
        validationDispute: { ...terms, ...amend },
      }),
    ).toThrow(refusal);
  });

  it("refuses the VerifySource authority on another category's workflow", async () => {
    const runtime = await runtimeFunding("verify_source", { useStage: true });
    const signed = signedFundingTransaction({
      inputOutRefs: runtime.selected.fundingOutRefs,
      outputLovelace: 14_800_000n,
    });
    const transaction = (prepared: typeof signed) =>
      runtime.prepareTransaction({
        action: {
          actionId: "verify_source",
          input: { stage: "verify_source" },
        },
        preflight: bindWorkflowPreflightTransaction(
          Object.freeze({ txHash: prepared.toHash() }),
          prepared,
        ),
      });
    grantValidationDisputeFundingAuthority({
      signed,
      deploymentFingerprint: DEPLOYMENT,
      headerHash: "a7".repeat(32),
      opener: {
        txHash: "75".repeat(32),
        outputIndex: 0,
        address: fundingAddress,
        assets: { lovelace: 5_000_000n },
      },
      bond: {
        address: BOND_ADDRESS,
        unit: "d5".repeat(28),
        lovelace: 10_000_000_000n,
        datumCborHex: "00",
      },
      caseOutput: {
        address: CASE_ADDRESS,
        unit: "d5".repeat(28),
        floorLovelace: 3_200_000n,
        lovelace: 5_000_000n,
      },
    });
    await expect(transaction(signed)).rejects.toThrow(
      "validation dispute funding authority differs from its exact VerifySource action or deployment terms (category, policy category, dispute terms,",
    );
    // The same transaction without the authority is the category's own.
    const unauthorised = await runtimeFunding("verify_source", {
      useStage: true,
    });
    await expect(
      unauthorised.prepareTransaction({
        action: {
          actionId: "verify_source",
          input: { stage: "verify_source" },
        },
        preflight: bindWorkflowPreflightTransaction(
          Object.freeze({ txHash: "stage-funded" }),
          signedFundingTransaction({
            inputOutRefs: unauthorised.selected.fundingOutRefs,
            outputLovelace: 14_800_000n,
          }),
        ),
      }),
    ).resolves.toBeUndefined();
  });
});
