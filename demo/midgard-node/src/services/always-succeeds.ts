import * as SDK from "@al-ft/midgard-sdk";
import {
  applyDoubleCborEncoding,
  MintingPolicy,
  mintingPolicyToId,
  Network,
  SpendingValidator,
  validatorToAddress,
  validatorToScriptHash,
  WithdrawalValidator,
} from "@lucid-evolution/lucid";
import { Effect, pipe } from "effect";
import { NoSuchElementException } from "effect/Cause";

import * as scripts from "../../blueprints/always-succeeds/plutus.json" with { type: "json" };

/**
 * Always-succeeds contract loader used for local development and testing.
 *
 * This service resolves validators from the blueprint bundle and converts them
 * into the SDK shapes expected by the rest of the node.
 */
const NETWORK: Network = "Preprod";

type Category = "midgard" | "fraud_proofs";

type Purpose = "spend" | "mint" | "withdraw";

/**
 * Builds the validator title used inside the blueprint JSON bundle.
 */
const makeValidatorTitle = (
  category: Category,
  baseName: string,
  type: Purpose,
) =>
  category === "midgard"
    ? `${category}.${baseName}_${type}.else`
    : `${category}.${baseName}.else`;

/**
 * Looks up a compiled validator script by title inside the blueprint bundle.
 */
const getValidatorScript = (title: string) =>
  pipe(
    Effect.fromNullable(
      scripts.default.validators.find((v) => v.title === title),
    ),
    Effect.andThen((script) => script.compiledCode),
  );

/**
 * Constructs an SDK spending validator from the blueprint bundle.
 */
const makeSpendingValidator = (
  category: Category,
  baseName: string,
  network: Network,
): Effect.Effect<SDK.SpendingValidator, NoSuchElementException> =>
  Effect.gen(function* () {
    const spendingScriptCBOR = yield* getValidatorScript(
      makeValidatorTitle(category, baseName, "spend"),
    );

    const spendingScript: SpendingValidator = {
      type: "PlutusV3",
      script: applyDoubleCborEncoding(spendingScriptCBOR),
    };

    const spendingScriptAddress = validatorToAddress(network, spendingScript);
    const spendingScriptHash = validatorToScriptHash(spendingScript);

    return {
      spendingScriptCBOR,
      spendingScript,
      spendingScriptAddress,
      spendingScriptHash,
    };
  }).pipe(
    Effect.tapError((_e) =>
      Effect.logError(`Failed to load validator: ${baseName}`),
    ),
  );

/**
 * Constructs an SDK minting validator from the blueprint bundle.
 */
const makeMintingValidator = (
  category: Category,
  baseName: string,
): Effect.Effect<SDK.MintingValidator, NoSuchElementException> =>
  Effect.gen(function* () {
    const mintingScriptCBOR = yield* getValidatorScript(
      makeValidatorTitle(category, baseName, "mint"),
    );

    const mintingScript: MintingPolicy = {
      type: "PlutusV3",
      script: applyDoubleCborEncoding(mintingScriptCBOR),
    };

    const policyId = mintingPolicyToId(mintingScript);

    return {
      mintingScriptCBOR,
      mintingScript,
      policyId,
    };
  }).pipe(
    Effect.tapError((_e) =>
      Effect.logError(`Failed to load validator: ${baseName}`),
    ),
  );

/**
 * Constructs an SDK withdrawal validator from the blueprint bundle.
 */
const makeWithdrawalValidator = (
  category: Category,
  baseName: string,
): Effect.Effect<SDK.WithdrawalValidator, NoSuchElementException> =>
  Effect.gen(function* () {
    const withdrawalScriptCBOR = yield* getValidatorScript(
      makeValidatorTitle(category, baseName, "withdraw"),
    );

    const withdrawalScript: WithdrawalValidator = {
      type: "PlutusV3",
      script: applyDoubleCborEncoding(withdrawalScriptCBOR),
    };

    const withdrawalScriptHash = validatorToScriptHash(withdrawalScript);

    return {
      withdrawalScriptCBOR,
      withdrawalScript,
      withdrawalScriptHash,
    };
  }).pipe(
    Effect.tapError((_e) =>
      Effect.logError(`Failed to load validator: ${baseName}`),
    ),
  );

/**
 * Builds the authenticated validator bundle used by most Midgard state
 * machines.
 */
const makeAuthenticatedValidator = (
  baseName: string,
  network: Network,
): Effect.Effect<SDK.AuthenticatedValidator, NoSuchElementException> =>
  Effect.gen(function* () {
    return {
      ...(yield* makeSpendingValidator("midgard", baseName, network)),
      ...(yield* makeMintingValidator("midgard", baseName)),
    };
  }).pipe(
    Effect.tapError((_e) =>
      Effect.logError(`Failed to load validator: ${baseName}`),
    ),
  );

const makeMintOnlyAuthenticatedValidator = (
  baseName: string,
  network: Network,
): Effect.Effect<SDK.AuthenticatedValidator, NoSuchElementException> =>
  Effect.gen(function* () {
    const mintingValidator = yield* makeMintingValidator("midgard", baseName);
    const spendingScript: SpendingValidator = {
      type: "PlutusV3",
      script: mintingValidator.mintingScript.script,
    };

    return {
      spendingScriptCBOR: mintingValidator.mintingScriptCBOR,
      spendingScript,
      spendingScriptAddress: validatorToAddress(network, spendingScript),
      spendingScriptHash: validatorToScriptHash(spendingScript),
      ...mintingValidator,
    };
  });

const repeatedFaultProofChain = <Chain extends SDK.FraudProofChain>(
  validator: SDK.SpendingValidator,
  stepCount: number,
): Chain =>
  ({
    firstStep: validator,
    steps: Array.from({ length: stepCount }, () => validator),
  }) as unknown as Chain;

/**
 * Resolves the full always-succeeds validator set used by test environments.
 */
const makeAlwaysSucceedsService: Effect.Effect<SDK.MidgardValidators> =
  Effect.gen(function* () {
    // Helpers
    /**
     * Builds the authenticated always-succeeds validator reference.
     */
    const mkAuthVal = (contract: string) =>
      makeAuthenticatedValidator(contract, NETWORK);
    /**
     * Builds the always-succeeds fraud-proof validator reference.
     */
    const mkFP = (fp: string) =>
      makeSpendingValidator("fraud_proofs", fp, NETWORK);

    // Midgard Contracts
    const hubOracle = yield* makeMintOnlyAuthenticatedValidator(
      "hub_oracle",
      NETWORK,
    );
    const referenceScriptAuth = yield* makeMintingValidator(
      "midgard",
      "state_queue",
    );
    const scheduler = yield* mkAuthVal("scheduler");
    const stateQueue = yield* mkAuthVal("state_queue");
    const correctionLock = stateQueue;
    const daParamsGovernor = stateQueue;
    const daAttestation = stateQueue;
    const availabilityChallenge = stateQueue;
    const registeredOperators = yield* mkAuthVal("registered_operators");
    const activeOperators = yield* mkAuthVal("active_operators");
    const retiredOperators = yield* mkAuthVal("retired_operators");
    const escapeHatch = yield* mkAuthVal("escape_hatch");
    const fraudProofCatalogue = yield* mkAuthVal("fraud_proof_catalogue");
    const fraudProof = yield* mkAuthVal("fraud_proof");
    const deposit = yield* mkAuthVal("deposit");
    const payout = yield* mkAuthVal("payout");
    const withdrawal = yield* mkAuthVal("withdrawal");
    const txOrder = yield* mkAuthVal("tx_order");
    // #579 retired the `txOrderFieldPreimage` and `txOrderFieldReceipt`
    // stand-ins with the tx-field family itself. This one always-succeeds spend
    // validator is all that survives of them, and it now serves only the two
    // roles that are still deployed. This service exists to give the node a
    // contract set that always succeeds, so the §8.6 certificate below is a
    // spend+mint pair like every other authenticated role, not a real
    // certificate policy.
    const alwaysSucceedsSpend = yield* makeSpendingValidator(
      "midgard",
      "tx_order",
      NETWORK,
    );
    const fieldPreimageCertificate = {
      ...alwaysSucceedsSpend,
      mintingScriptCBOR: txOrder.mintingScriptCBOR,
      mintingScript: txOrder.mintingScript,
      policyId: txOrder.policyId,
    };
    const cekProgramMaterial = alwaysSucceedsSpend;
    const settlement = yield* mkAuthVal("settlement");

    // Reserve
    // TODO: Reserve is not entirely defined in specs. This might change.
    const reserveSpendingValidator = yield* makeSpendingValidator(
      "midgard",
      "reserve",
      NETWORK,
    );
    const reserveWithdawalValidator = yield* makeWithdrawalValidator(
      "midgard",
      "reserve",
    );
    const reserve = {
      ...reserveSpendingValidator,
      ...reserveWithdawalValidator,
    };

    // Fraud Proofs
    const doubleSpend = yield* mkFP("double_spend");
    const nonExistentInput = yield* mkFP("non_existent_input");
    const nonExistentInputNoIndex = yield* mkFP("non_existent_input_no_index");
    const invalidRange = yield* mkFP("invalid_range");
    const transitionTrace = yield* mkFP("transition_trace");
    const zeroInput = yield* mkFP("zero_input");
    const validationTraceDispute: SDK.ValidationTraceDisputeValidators = {
      ...transitionTrace,
      source: transitionTrace,
      game: transitionTrace,
      boundary: transitionTrace,
      timeout: transitionTrace,
      award: transitionTrace,
    };

    // The always-succeeds devnet blueprint has no dedicated
    // `da_hash_preimage` stub validator, so the Q44 category reuses the
    // `zero_input` stub here, exactly as `validationTraceDispute` reuses the
    // `transition_trace` stub above. Devnet only: the production contracts
    // service builds the real Q44 chain from the Aiken blueprint.
    const daHashPreimage = zeroInput;

    // Same devnet aliasing for the three families registered by #547: the
    // always-succeeds blueprint carries no `no_reference_input`,
    // `reference_input_no_idx`, or `invalid_signature` stub, so each reuses
    // the stub of the family it mirrors on-chain. Devnet only: the production
    // contracts service builds the real chains from the Aiken blueprint.
    const noReferenceInput = nonExistentInput;
    const referenceInputNoIdx = nonExistentInputNoIndex;
    const invalidSignature = invalidRange;

    const validationTraceSemanticResolvers = Array.from(
      { length: 90 },
      () => transitionTrace,
    );
    const validationTracePrepareResolvers = Array.from(
      { length: 14 },
      () => transitionTrace,
    );
    const validationTraceCanonicalDecodeItemStages = {
      source: transitionTrace,
      observe: transitionTrace,
      proof: transitionTrace,
      settlement: transitionTrace,
    };
    const validationTraceScriptSourcesStageOneRedeemerStages = {
      envelope: transitionTrace,
      traversalNormalizer: transitionTrace,
      outerNormalizer: transitionTrace,
      foldMapExecutor: transitionTrace,
      finalizeFrameExecutor: transitionTrace,
      settlement: transitionTrace,
    };
    const validationTraceDisputeChain = {
      firstStep: transitionTrace,
      steps: [
        transitionTrace,
        validationTraceDispute.source,
        validationTraceDispute.game,
        validationTraceDispute.boundary,
        validationTraceDispute.timeout,
        validationTraceDispute.award,
        transitionTrace,
        ...validationTraceSemanticResolvers,
        transitionTrace,
        transitionTrace,
        transitionTrace,
        transitionTrace,
        transitionTrace,
        ...Object.values(validationTraceCanonicalDecodeItemStages),
        ...validationTracePrepareResolvers,
      ],
      cekProgramMaterial,
      opener: validationTraceDispute,
      source: validationTraceDispute.source,
      game: validationTraceDispute.game,
      boundary: validationTraceDispute.boundary,
      timeout: validationTraceDispute.timeout,
      award: validationTraceDispute.award,
      proofItem: transitionTrace,
      canonicalDecodeItemStages: validationTraceCanonicalDecodeItemStages,
      scriptSourcesStageOneRedeemerStages:
        validationTraceScriptSourcesStageOneRedeemerStages,
      prepareResolvers: validationTracePrepareResolvers,
      semanticResolvers: validationTraceSemanticResolvers,
      resolvers: validationTracePrepareResolvers,
    } as unknown as SDK.FaultProofContractChains["validationTraceDispute"];
    const transitionTraceFinals = [
      transitionTrace,
      transitionTrace,
      transitionTrace,
      transitionTrace,
      transitionTrace,
      transitionTrace,
      transitionTrace,
      transitionTrace,
    ] as const;
    const fraudProofContracts: SDK.FaultProofContractChains = {
      doubleSpend: repeatedFaultProofChain(doubleSpend, 4),
      nonExistentInput: repeatedFaultProofChain(nonExistentInput, 4),
      nonExistentInputNoIndex: repeatedFaultProofChain(
        nonExistentInputNoIndex,
        4,
      ),
      invalidRange: repeatedFaultProofChain(invalidRange, 2),
      transitionTrace: {
        firstStep: transitionTrace,
        route: transitionTrace,
        finals: transitionTraceFinals,
        steps: [transitionTrace, ...transitionTraceFinals],
      },
      zeroInput: repeatedFaultProofChain(zeroInput, 2),
      validationTraceDispute: validationTraceDisputeChain,
      daHashPreimage: repeatedFaultProofChain(daHashPreimage, 2),
      noReferenceInput: repeatedFaultProofChain(noReferenceInput, 4),
      referenceInputNoIdx: repeatedFaultProofChain(referenceInputNoIdx, 4),
      invalidSignature: repeatedFaultProofChain(invalidSignature, 2),
      fabricatedDeposit: repeatedFaultProofChain(zeroInput, 4),
      fabricatedWithdrawal: repeatedFaultProofChain(zeroInput, 4),
      nativeScriptDecoding: repeatedFaultProofChain(zeroInput, 6),
      missingSignature: repeatedFaultProofChain(invalidSignature, 4),
      missingNativeScriptTx: repeatedFaultProofChain(zeroInput, 8),
      withdrawnReferenceInput: repeatedFaultProofChain(referenceInputNoIdx, 3),
      canonicalDecodability: repeatedFaultProofChain(zeroInput, 2),
      committedFieldShape: repeatedFaultProofChain(zeroInput, 2),
      minFee: repeatedFaultProofChain(invalidRange, 2),
      withdrawalMistag: repeatedFaultProofChain(invalidRange, 5),
      doubleWithdraw: repeatedFaultProofChain(doubleSpend, 2),
      crossBlockDuplicateEvent: repeatedFaultProofChain(doubleSpend, 2),
      l2TxMistag: repeatedFaultProofChain(invalidRange, 2),
      withdrawnInput: repeatedFaultProofChain(nonExistentInput, 3),
      valueNotPreserved: repeatedFaultProofChain(zeroInput, 4),
      inputSetUniqueness: repeatedFaultProofChain(doubleSpend, 2),
      mintAuthorization: repeatedFaultProofChain(zeroInput, 5),
      networkId: repeatedFaultProofChain(invalidRange, 2),
      missingNativeScriptUtxo: repeatedFaultProofChain(zeroInput, 5),
      nativeScriptInvalid: repeatedFaultProofChain(zeroInput, 3),
      minAda: repeatedFaultProofChain(invalidRange, 5),
    };
    const fraudProofs =
      SDK.fraudProofContractsToFirstSteps(fraudProofContracts);

    return {
      referenceScriptAuth,
      hubOracle,
      daParamsGovernor,
      daAttestation,
      availabilityChallenge,
      correctionLock,
      stateQueue,
      scheduler,
      registeredOperators,
      activeOperators,
      retiredOperators,
      escapeHatch,
      fraudProofCatalogue,
      computationThread: fraudProof,
      fraudProof,
      chunkedVerify: reserveWithdawalValidator,
      pexcludes: reserveWithdawalValidator,
      deposit,
      withdrawal,
      txOrder,
      fieldPreimageCertificate,
      cekProgramMaterial,
      settlement,
      reserve,
      payout,
      fraudProofContracts,
      fraudProofs,
    };
  }).pipe(Effect.orDie);

export class AlwaysSucceedsContract extends Effect.Service<AlwaysSucceedsContract>()(
  "AlwaysSucceedsContract",
  {
    effect: makeAlwaysSucceedsService,
  },
) {}
