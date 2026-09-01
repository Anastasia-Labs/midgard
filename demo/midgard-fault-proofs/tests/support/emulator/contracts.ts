import {
  AddressData,
  addressDataFromBech32,
  type AuthenticatedValidator,
  buildCanonicalDecodabilityFaultProofContracts,
  buildCommittedFieldShapeFaultProofContracts,
  buildCrossBlockDuplicateEventFaultProofContracts,
  buildDaHashPreimageFaultProofContracts,
  buildDoubleSpendFaultProofContracts,
  buildDoubleWithdrawFaultProofContracts,
  buildFabricatedDepositFaultProofContracts,
  buildFabricatedWithdrawalFaultProofContracts,
  buildInputNoIdxFaultProofContracts,
  buildInputSetUniquenessFaultProofContracts,
  buildInvalidRangeFaultProofContracts,
  buildInvalidSignatureFaultProofContracts,
  buildL2TxMistagFaultProofContracts,
  buildMinAdaFaultProofContracts,
  buildMinFeeFaultProofContracts,
  buildMintAuthorizationFaultProofContracts,
  buildMissingNativeScriptTxFaultProofContracts,
  buildMissingNativeScriptUtxoFaultProofContracts,
  buildMissingSignatureFaultProofContracts,
  buildNativeScriptDecodingFaultProofContracts,
  buildNativeScriptInvalidFaultProofContracts,
  buildNonExistentInputFaultProofContracts,
  buildNoReferenceInputFaultProofContracts,
  buildReferenceInputNoIdxFaultProofContracts,
  buildTransitionTraceFaultProofContracts,
  buildValidationTraceDisputeFaultProofContracts,
  buildValueNotPreservedFaultProofContracts,
  buildWithdrawalMistagFaultProofContracts,
  buildWithdrawnInputFaultProofContracts,
  buildWithdrawnReferenceInputFaultProofContracts,
  buildZeroInputFaultProofContracts,
  FABRICATED_DEPOSIT_FRAUD_CATEGORY_ID_V1,
  FABRICATED_WITHDRAWAL_FRAUD_CATEGORY_ID_V1,
  FAULT_PROOF_SHARED_TITLES,
  type FaultProofContractChains,
  fraudProofContractsToFirstSteps,
  HUB_ORACLE_ASSET_NAME,
  type MidgardValidators,
  type MintingValidator as SdkMintingValidator,
  parseFaultProofBlueprint,
  type SpendingValidator as SdkSpendingValidator,
} from "@al-ft/midgard-sdk";
import {
  Constr,
  credentialToAddress,
  Data,
  scriptHashToCredential,
  type SpendingValidator,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import {
  CANONICAL_DECODABILITY_BLUEPRINT_TITLES_V1,
  type CanonicalDecodabilityContractsV1,
} from "../../../src/canonical-decodability/contracts-v1.js";
import {
  COMMITTED_FIELD_SHAPE_BLUEPRINT_TITLES_V1,
  type CommittedFieldShapeContractsV1,
} from "../../../src/committed-field-shape/contracts-v1.js";
import {
  CROSS_BLOCK_DUPLICATE_EVENT_BLUEPRINT_TITLES_V1,
  type CrossBlockDuplicateEventContractsV1,
} from "../../../src/cross-block-duplicate-event/index.js";
import {
  DOUBLE_WITHDRAW_BLUEPRINT_TITLES_V1,
  type DoubleWithdrawContractsV1,
} from "../../../src/double-withdraw/contracts-v1.js";
import {
  INPUT_SET_UNIQUENESS_BLUEPRINT_TITLES_V1,
  type InputSetUniquenessContractsV1,
} from "../../../src/input-set-uniqueness/contracts-v1.js";
import { type L2TxMistagContractsV1 } from "../../../src/l2-tx-mistag/contracts-v1.js";
import { type MinAdaContractsV1 } from "../../../src/min-ada/contracts-v1.js";
import { type MinFeeContractsV1 } from "../../../src/min-fee-contracts-v1.js";
import {
  MINT_AUTHORIZATION_BLUEPRINT_TITLES_V1,
  type MintAuthorizationContractsV1,
} from "../../../src/mint-authorization/contracts-v1.js";
import {
  MISSING_NATIVE_SCRIPT_TX_BLUEPRINT_TITLES_V1,
  type MissingNativeScriptTxContractsV1,
} from "../../../src/missing-native-script-tx/contracts-v1.js";
import { type MissingNativeScriptUtxoContractsV1 } from "../../../src/missing-native-script-utxo/contracts-v1.js";
import {
  MISSING_SIGNATURE_BLUEPRINT_TITLES_V1,
  type MissingSignatureContractsV1,
} from "../../../src/missing-signature/contracts-v1.js";
import {
  NATIVE_SCRIPT_DECODING_BLUEPRINT_TITLES_V1,
  type NativeScriptDecodingContractsV1,
} from "../../../src/native-script-decoding/contracts-v1.js";
import { type NativeScriptInvalidContractsV1 } from "../../../src/native-script-invalid/contracts-v1.js";
import { type FabricatedDepositContractsV1 } from "../../../src/submit-fabricated-deposit-step-01.js";
import { type FabricatedWithdrawalContractsV1 } from "../../../src/submit-fabricated-withdrawal-step-01.js";
import {
  VALUE_NOT_PRESERVED_BLUEPRINT_TITLES_V1,
  type ValueNotPreservedContractsV1,
} from "../../../src/value-not-preserved/contracts-v1.js";
import {
  WITHDRAWAL_MISTAG_BLUEPRINT_TITLES_V1,
  type WithdrawalMistagContractsV1,
} from "../../../src/withdrawal-mistag/contracts-v1.js";
import {
  WITHDRAWN_INPUT_BLUEPRINT_TITLES_V1,
  type WithdrawnInputContractsV1,
} from "../../../src/withdrawn-input/contracts-v1.js";
import {
  WITHDRAWN_REFERENCE_INPUT_BLUEPRINT_TITLES_V1,
  type WithdrawnReferenceInputContractsV1,
} from "../../../src/withdrawn-reference-input/contracts-v1.js";
import {
  applyCompiledScript,
  type Blueprint,
  cloneBlueprint,
  getCompiledScript,
  network,
} from "./blueprints.js";
import {} from "./catalogue.js";
import {
  makeAlwaysSucceedsContracts,
  makeAuthenticatedValidator,
  makeIsolatedAlwaysSucceedsAuthenticatedValidator,
  makeMintingValidator,
  makeSpendingValidator,
  makeWithdrawalValidator,
} from "./validators.js";

type EmulatorStepTuple = readonly [
  Pick<SdkSpendingValidator, "spendingScript">,
  ...Pick<SdkSpendingValidator, "spendingScript">[],
];

type SdkStepTuple<Steps extends EmulatorStepTuple> = {
  readonly [Index in keyof Steps]: SdkSpendingValidator;
};

const chainFromSteps = <const Steps extends EmulatorStepTuple>(
  steps: Steps,
) => {
  const sdkSteps = steps.map((step) =>
    makeSpendingValidator(step.spendingScript.script),
  ) as unknown as SdkStepTuple<Steps>;
  return { firstStep: sdkSteps[0], steps: sdkSteps };
};

/** Applies step 02 first, then pins its hash into step 01 in blueprint order. */
export const buildCommittedFieldShapeChainV1 = ({
  realBlueprint,
  computationThreadPolicyId,
  fraudProofPolicyId,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
  hubOraclePolicyId,
}: {
  readonly realBlueprint: Blueprint;
  readonly computationThreadPolicyId: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofTokenAddressData: Data;
  readonly fieldPreimageCertificatePolicyId: string;
  readonly hubOraclePolicyId: string;
}): readonly [SdkSpendingValidator, SdkSpendingValidator] => {
  const step02 = makeSpendingValidator(
    applyCompiledScript(
      realBlueprint,
      COMMITTED_FIELD_SHAPE_BLUEPRINT_TITLES_V1.step02,
      [
        fraudProofPolicyId,
        fraudProofTokenAddressData,
        computationThreadPolicyId,
      ],
    ),
  );
  const step01 = makeSpendingValidator(
    applyCompiledScript(
      realBlueprint,
      COMMITTED_FIELD_SHAPE_BLUEPRINT_TITLES_V1.step01,
      [
        step02.spendingScriptHash,
        computationThreadPolicyId,
        hubOraclePolicyId,
        fieldPreimageCertificatePolicyId,
      ],
    ),
  );
  return [step01, step02];
};

/** Applies the two-step `double-withdraw` chain backwards. */
export const buildDoubleWithdrawChainV1 = ({
  realBlueprint,
  computationThreadPolicyId,
  fraudProofPolicyId,
  fraudProofTokenAddressData,
  hubOraclePolicyId,
}: {
  readonly realBlueprint: Blueprint;
  readonly computationThreadPolicyId: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofTokenAddressData: Data;
  readonly hubOraclePolicyId: string;
}): readonly [SdkSpendingValidator, SdkSpendingValidator] => {
  const step02 = makeSpendingValidator(
    applyCompiledScript(
      realBlueprint,
      DOUBLE_WITHDRAW_BLUEPRINT_TITLES_V1.step02,
      [
        fraudProofPolicyId,
        fraudProofTokenAddressData,
        computationThreadPolicyId,
        hubOraclePolicyId,
      ],
    ),
  );
  const step01 = makeSpendingValidator(
    applyCompiledScript(
      realBlueprint,
      DOUBLE_WITHDRAW_BLUEPRINT_TITLES_V1.step01,
      [step02.spendingScriptHash, computationThreadPolicyId, hubOraclePolicyId],
    ),
  );
  return [step01, step02];
};

/** Apply the missing-signature chain backwards in blueprint parameter order. */
export const buildMissingSignatureChainV1 = ({
  realBlueprint,
  computationThreadPolicyId,
  fraudProofPolicyId,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
  hubOraclePolicyId,
}: {
  readonly realBlueprint: Blueprint;
  readonly computationThreadPolicyId: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofTokenAddressData: Data;
  readonly fieldPreimageCertificatePolicyId: string;
  readonly hubOraclePolicyId: string;
}): MissingSignatureContractsV1["steps"] => {
  const step04 = makeSpendingValidator(
    applyCompiledScript(
      realBlueprint,
      MISSING_SIGNATURE_BLUEPRINT_TITLES_V1.step04,
      [
        fraudProofPolicyId,
        fraudProofTokenAddressData,
        computationThreadPolicyId,
        fieldPreimageCertificatePolicyId,
      ],
    ),
  );
  const step03 = makeSpendingValidator(
    applyCompiledScript(
      realBlueprint,
      MISSING_SIGNATURE_BLUEPRINT_TITLES_V1.step03,
      [step04.spendingScriptHash, computationThreadPolicyId],
    ),
  );
  const step02 = makeSpendingValidator(
    applyCompiledScript(
      realBlueprint,
      MISSING_SIGNATURE_BLUEPRINT_TITLES_V1.step02,
      [
        step03.spendingScriptHash,
        computationThreadPolicyId,
        fieldPreimageCertificatePolicyId,
      ],
    ),
  );
  const step01 = makeSpendingValidator(
    applyCompiledScript(
      realBlueprint,
      MISSING_SIGNATURE_BLUEPRINT_TITLES_V1.step01,
      [step02.spendingScriptHash, computationThreadPolicyId, hubOraclePolicyId],
    ),
  );
  return [step01, step02, step03, step04];
};

/**
 * Applies the six-validator `native-script-decoding` chain in
 * blueprint-declared parameter order. It is built backwards because every
 * custody step is parameterized by its successor's script hash.
 */
export const buildNativeScriptDecodingChainV1 = ({
  realBlueprint,
  computationThreadPolicyId,
  fraudProofPolicyId,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
  hubOraclePolicyId,
}: {
  readonly realBlueprint: Blueprint;
  readonly computationThreadPolicyId: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofTokenAddressData: Data;
  readonly fieldPreimageCertificatePolicyId: string;
  readonly hubOraclePolicyId: string;
}): readonly [
  SdkSpendingValidator,
  SdkSpendingValidator,
  SdkSpendingValidator,
  SdkSpendingValidator,
  SdkSpendingValidator,
  SdkSpendingValidator,
] => {
  const step04 = makeSpendingValidator(
    applyCompiledScript(
      realBlueprint,
      NATIVE_SCRIPT_DECODING_BLUEPRINT_TITLES_V1.step04,
      [
        computationThreadPolicyId,
        fraudProofPolicyId,
        fraudProofTokenAddressData,
      ],
    ),
  );
  const step03AdvanceOrClose = makeSpendingValidator(
    applyCompiledScript(
      realBlueprint,
      NATIVE_SCRIPT_DECODING_BLUEPRINT_TITLES_V1.step03AdvanceOrClose,
      [step04.spendingScriptHash, computationThreadPolicyId],
    ),
  );
  const step03BindDescriptor = makeSpendingValidator(
    applyCompiledScript(
      realBlueprint,
      NATIVE_SCRIPT_DECODING_BLUEPRINT_TITLES_V1.step03BindDescriptor,
      [
        step03AdvanceOrClose.spendingScriptHash,
        step04.spendingScriptHash,
        computationThreadPolicyId,
      ],
    ),
  );
  const step03OpenSubject = makeSpendingValidator(
    applyCompiledScript(
      realBlueprint,
      NATIVE_SCRIPT_DECODING_BLUEPRINT_TITLES_V1.step03OpenSubject,
      [
        step03BindDescriptor.spendingScriptHash,
        step04.spendingScriptHash,
        computationThreadPolicyId,
        fieldPreimageCertificatePolicyId,
      ],
    ),
  );
  const step02 = makeSpendingValidator(
    applyCompiledScript(
      realBlueprint,
      NATIVE_SCRIPT_DECODING_BLUEPRINT_TITLES_V1.step02,
      [step03OpenSubject.spendingScriptHash, computationThreadPolicyId],
    ),
  );
  const step01 = makeSpendingValidator(
    applyCompiledScript(
      realBlueprint,
      NATIVE_SCRIPT_DECODING_BLUEPRINT_TITLES_V1.step01,
      [step02.spendingScriptHash, computationThreadPolicyId, hubOraclePolicyId],
    ),
  );
  return [
    step01,
    step02,
    step03OpenSubject,
    step03BindDescriptor,
    step03AdvanceOrClose,
    step04,
  ];
};

/** Applies the eight validators backwards in their blueprint-declared order. */
export const buildMissingNativeScriptTxChainV1 = ({
  realBlueprint,
  computationThreadPolicyId,
  fraudProofPolicyId,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
  hubOraclePolicyId,
}: {
  readonly realBlueprint: Blueprint;
  readonly computationThreadPolicyId: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofTokenAddressData: Data;
  readonly fieldPreimageCertificatePolicyId: string;
  readonly hubOraclePolicyId: string;
}): readonly [
  SdkSpendingValidator,
  SdkSpendingValidator,
  SdkSpendingValidator,
  SdkSpendingValidator,
  SdkSpendingValidator,
  SdkSpendingValidator,
  SdkSpendingValidator,
  SdkSpendingValidator,
] => {
  const step08 = makeSpendingValidator(
    applyCompiledScript(
      realBlueprint,
      MISSING_NATIVE_SCRIPT_TX_BLUEPRINT_TITLES_V1.step08,
      [
        computationThreadPolicyId,
        fraudProofPolicyId,
        fraudProofTokenAddressData,
        fieldPreimageCertificatePolicyId,
      ],
    ),
  );
  const step07 = makeSpendingValidator(
    applyCompiledScript(
      realBlueprint,
      MISSING_NATIVE_SCRIPT_TX_BLUEPRINT_TITLES_V1.step07,
      [
        step08.spendingScriptHash,
        computationThreadPolicyId,
        fieldPreimageCertificatePolicyId,
      ],
    ),
  );
  const step06 = makeSpendingValidator(
    applyCompiledScript(
      realBlueprint,
      MISSING_NATIVE_SCRIPT_TX_BLUEPRINT_TITLES_V1.step06,
      [
        step07.spendingScriptHash,
        computationThreadPolicyId,
        fraudProofPolicyId,
        fraudProofTokenAddressData,
        fieldPreimageCertificatePolicyId,
      ],
    ),
  );
  const step05 = makeSpendingValidator(
    applyCompiledScript(
      realBlueprint,
      MISSING_NATIVE_SCRIPT_TX_BLUEPRINT_TITLES_V1.step05,
      [step06.spendingScriptHash, computationThreadPolicyId],
    ),
  );
  const step04 = makeSpendingValidator(
    applyCompiledScript(
      realBlueprint,
      MISSING_NATIVE_SCRIPT_TX_BLUEPRINT_TITLES_V1.step04,
      [
        step05.spendingScriptHash,
        computationThreadPolicyId,
        fieldPreimageCertificatePolicyId,
      ],
    ),
  );
  const step03 = makeSpendingValidator(
    applyCompiledScript(
      realBlueprint,
      MISSING_NATIVE_SCRIPT_TX_BLUEPRINT_TITLES_V1.step03,
      [step04.spendingScriptHash, computationThreadPolicyId, hubOraclePolicyId],
    ),
  );
  const step02 = makeSpendingValidator(
    applyCompiledScript(
      realBlueprint,
      MISSING_NATIVE_SCRIPT_TX_BLUEPRINT_TITLES_V1.step02,
      [
        step03.spendingScriptHash,
        computationThreadPolicyId,
        fieldPreimageCertificatePolicyId,
      ],
    ),
  );
  const step01 = makeSpendingValidator(
    applyCompiledScript(
      realBlueprint,
      MISSING_NATIVE_SCRIPT_TX_BLUEPRINT_TITLES_V1.step01,
      [step02.spendingScriptHash, computationThreadPolicyId, hubOraclePolicyId],
    ),
  );
  return [step01, step02, step03, step04, step05, step06, step07, step08];
};

/** Applies the two-step canonical-decodability family backwards. */
export const buildCanonicalDecodabilityChainV1 = ({
  realBlueprint,
  computationThreadPolicyId,
  fraudProofPolicyId,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
  hubOraclePolicyId,
}: {
  readonly realBlueprint: Blueprint;
  readonly computationThreadPolicyId: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofTokenAddressData: Data;
  readonly fieldPreimageCertificatePolicyId: string;
  readonly hubOraclePolicyId: string;
}): readonly [SdkSpendingValidator, SdkSpendingValidator] => {
  const step02 = makeSpendingValidator(
    applyCompiledScript(
      realBlueprint,
      CANONICAL_DECODABILITY_BLUEPRINT_TITLES_V1.step02,
      [
        fraudProofPolicyId,
        fraudProofTokenAddressData,
        computationThreadPolicyId,
      ],
    ),
  );
  const step01 = makeSpendingValidator(
    applyCompiledScript(
      realBlueprint,
      CANONICAL_DECODABILITY_BLUEPRINT_TITLES_V1.step01,
      [
        step02.spendingScriptHash,
        computationThreadPolicyId,
        hubOraclePolicyId,
        fieldPreimageCertificatePolicyId,
      ],
    ),
  );
  return [step01, step02];
};

export const buildCrossBlockDuplicateEventChainV1 = ({
  realBlueprint,
  computationThreadPolicyId,
  fraudProofPolicyId,
  fraudProofTokenAddressData,
  hubOraclePolicyId,
}: {
  readonly realBlueprint: Blueprint;
  readonly computationThreadPolicyId: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofTokenAddressData: Data;
  readonly hubOraclePolicyId: string;
}): readonly [SdkSpendingValidator, SdkSpendingValidator] => {
  const step02 = makeSpendingValidator(
    applyCompiledScript(
      realBlueprint,
      CROSS_BLOCK_DUPLICATE_EVENT_BLUEPRINT_TITLES_V1.step02,
      [
        fraudProofPolicyId,
        fraudProofTokenAddressData,
        computationThreadPolicyId,
      ],
    ),
  );
  const step01 = makeSpendingValidator(
    applyCompiledScript(
      realBlueprint,
      CROSS_BLOCK_DUPLICATE_EVENT_BLUEPRINT_TITLES_V1.step01,
      [step02.spendingScriptHash, computationThreadPolicyId, hubOraclePolicyId],
    ),
  );
  return [step01, step02];
};

/** Applies the three withdrawn-reference-input validators back-to-front. */
export const buildWithdrawnReferenceInputChainV1 = ({
  realBlueprint,
  computationThreadPolicyId,
  fraudProofPolicyId,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
  hubOraclePolicyId,
}: {
  readonly realBlueprint: Blueprint;
  readonly computationThreadPolicyId: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofTokenAddressData: Data;
  readonly fieldPreimageCertificatePolicyId: string;
  readonly hubOraclePolicyId: string;
}): readonly [
  SdkSpendingValidator,
  SdkSpendingValidator,
  SdkSpendingValidator,
] => {
  const step03 = makeSpendingValidator(
    applyCompiledScript(
      realBlueprint,
      WITHDRAWN_REFERENCE_INPUT_BLUEPRINT_TITLES_V1.step03,
      [
        fraudProofPolicyId,
        fraudProofTokenAddressData,
        computationThreadPolicyId,
      ],
    ),
  );
  const step02 = makeSpendingValidator(
    applyCompiledScript(
      realBlueprint,
      WITHDRAWN_REFERENCE_INPUT_BLUEPRINT_TITLES_V1.step02,
      [
        step03.spendingScriptHash,
        computationThreadPolicyId,
        fieldPreimageCertificatePolicyId,
      ],
    ),
  );
  const step01 = makeSpendingValidator(
    applyCompiledScript(
      realBlueprint,
      WITHDRAWN_REFERENCE_INPUT_BLUEPRINT_TITLES_V1.step01,
      [step02.spendingScriptHash, computationThreadPolicyId, hubOraclePolicyId],
    ),
  );
  return [step01, step02, step03];
};

/** Applies the three-step withdrawn-input chain backwards. */
export const buildWithdrawnInputChainV1 = ({
  realBlueprint,
  computationThreadPolicyId,
  fraudProofPolicyId,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
  hubOraclePolicyId,
}: {
  readonly realBlueprint: Blueprint;
  readonly computationThreadPolicyId: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofTokenAddressData: Data;
  readonly fieldPreimageCertificatePolicyId: string;
  readonly hubOraclePolicyId: string;
}): WithdrawnInputContractsV1["steps"] => {
  const step03 = makeSpendingValidator(
    applyCompiledScript(
      realBlueprint,
      WITHDRAWN_INPUT_BLUEPRINT_TITLES_V1.step03,
      [
        fraudProofPolicyId,
        fraudProofTokenAddressData,
        computationThreadPolicyId,
      ],
    ),
  );
  const step02 = makeSpendingValidator(
    applyCompiledScript(
      realBlueprint,
      WITHDRAWN_INPUT_BLUEPRINT_TITLES_V1.step02,
      [
        step03.spendingScriptHash,
        computationThreadPolicyId,
        fieldPreimageCertificatePolicyId,
      ],
    ),
  );
  const step01 = makeSpendingValidator(
    applyCompiledScript(
      realBlueprint,
      WITHDRAWN_INPUT_BLUEPRINT_TITLES_V1.step01,
      [step02.spendingScriptHash, computationThreadPolicyId, hubOraclePolicyId],
    ),
  );
  return [step01, step02, step03];
};

/** Applies the reserved five-step withdrawal-mistag chain backwards. */
export const buildWithdrawalMistagChainV1 = ({
  realBlueprint,
  computationThreadPolicyId,
  fraudProofPolicyId,
  fraudProofTokenAddressData,
  hubOraclePolicyId,
}: {
  readonly realBlueprint: Blueprint;
  readonly computationThreadPolicyId: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofTokenAddressData: Data;
  readonly hubOraclePolicyId: string;
}): WithdrawalMistagContractsV1["steps"] => {
  const step05 = makeSpendingValidator(
    applyCompiledScript(
      realBlueprint,
      WITHDRAWAL_MISTAG_BLUEPRINT_TITLES_V1.step05,
      [
        computationThreadPolicyId,
        fraudProofPolicyId,
        fraudProofTokenAddressData,
      ],
    ),
  );
  const step04 = makeSpendingValidator(
    applyCompiledScript(
      realBlueprint,
      WITHDRAWAL_MISTAG_BLUEPRINT_TITLES_V1.step04,
      [step05.spendingScriptHash, computationThreadPolicyId],
    ),
  );
  const step03 = makeSpendingValidator(
    applyCompiledScript(
      realBlueprint,
      WITHDRAWAL_MISTAG_BLUEPRINT_TITLES_V1.step03,
      [step04.spendingScriptHash, computationThreadPolicyId],
    ),
  );
  const step02 = makeSpendingValidator(
    applyCompiledScript(
      realBlueprint,
      WITHDRAWAL_MISTAG_BLUEPRINT_TITLES_V1.step02,
      [step03.spendingScriptHash, computationThreadPolicyId],
    ),
  );
  const step01 = makeSpendingValidator(
    applyCompiledScript(
      realBlueprint,
      WITHDRAWAL_MISTAG_BLUEPRINT_TITLES_V1.step01,
      [step02.spendingScriptHash, computationThreadPolicyId, hubOraclePolicyId],
    ),
  );
  return [step01, step02, step03, step04, step05];
};

/**
 * Applies the two-step `input-set-uniqueness` chain in blueprint-declared
 * parameter order (the order note lives on
 * `INPUT_SET_UNIQUENESS_BLUEPRINT_TITLES_V1`). Applied backwards, step 02
 * first, because step 01 is parameterized by its successor's script hash.
 * Both steps deploy as reference scripts in production per the standing
 * reference-script ruling.
 */
export const buildInputSetUniquenessChainV1 = ({
  realBlueprint,
  computationThreadPolicyId,
  fraudProofPolicyId,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
  hubOraclePolicyId,
}: {
  readonly realBlueprint: Blueprint;
  readonly computationThreadPolicyId: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofTokenAddressData: Data;
  readonly fieldPreimageCertificatePolicyId: string;
  readonly hubOraclePolicyId: string;
}): readonly [SdkSpendingValidator, SdkSpendingValidator] => {
  const step02 = makeSpendingValidator(
    applyCompiledScript(
      realBlueprint,
      INPUT_SET_UNIQUENESS_BLUEPRINT_TITLES_V1.step02,
      [
        fraudProofPolicyId,
        fraudProofTokenAddressData,
        computationThreadPolicyId,
        fieldPreimageCertificatePolicyId,
      ],
    ),
  );
  const step01 = makeSpendingValidator(
    applyCompiledScript(
      realBlueprint,
      INPUT_SET_UNIQUENESS_BLUEPRINT_TITLES_V1.step01,
      [step02.spendingScriptHash, computationThreadPolicyId, hubOraclePolicyId],
    ),
  );
  return [step01, step02];
};

/**
 * Applies the four-step `value-not-preserved` chain in blueprint-declared
 * parameter order (pinned on `VALUE_NOT_PRESERVED_BLUEPRINT_TITLES_V1`).
 * Applied backwards — step 04 first — because each step is parameterized by
 * its successor's script hash. Steps 02 and 03 both take the §8.6
 * field-preimage certificate policy (each opens committed fields through the
 * §8.8 door), and step 04 leads with the fraud-proof pair. All four steps
 * deploy as reference scripts in production per the standing
 * reference-script ruling.
 */
export const buildValueNotPreservedChainV1 = ({
  realBlueprint,
  computationThreadPolicyId,
  fraudProofPolicyId,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
  hubOraclePolicyId,
}: {
  readonly realBlueprint: Blueprint;
  readonly computationThreadPolicyId: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofTokenAddressData: Data;
  readonly fieldPreimageCertificatePolicyId: string;
  readonly hubOraclePolicyId: string;
}): ValueNotPreservedContractsV1["steps"] => {
  const step04 = makeSpendingValidator(
    applyCompiledScript(
      realBlueprint,
      VALUE_NOT_PRESERVED_BLUEPRINT_TITLES_V1.step04,
      [
        fraudProofPolicyId,
        fraudProofTokenAddressData,
        computationThreadPolicyId,
      ],
    ),
  );
  const step03 = makeSpendingValidator(
    applyCompiledScript(
      realBlueprint,
      VALUE_NOT_PRESERVED_BLUEPRINT_TITLES_V1.step03,
      [
        step04.spendingScriptHash,
        computationThreadPolicyId,
        fieldPreimageCertificatePolicyId,
      ],
    ),
  );
  const step02 = makeSpendingValidator(
    applyCompiledScript(
      realBlueprint,
      VALUE_NOT_PRESERVED_BLUEPRINT_TITLES_V1.step02,
      [
        step03.spendingScriptHash,
        computationThreadPolicyId,
        fieldPreimageCertificatePolicyId,
      ],
    ),
  );
  const step01 = makeSpendingValidator(
    applyCompiledScript(
      realBlueprint,
      VALUE_NOT_PRESERVED_BLUEPRINT_TITLES_V1.step01,
      [step02.spendingScriptHash, computationThreadPolicyId, hubOraclePolicyId],
    ),
  );
  return [step01, step02, step03, step04];
};

/**
 * Applies the five-step `mint-authorization` chain in blueprint-declared
 * parameter order (see `MINT_AUTHORIZATION_BLUEPRINT_TITLES_V1` and the order
 * note on `MintAuthorizationContractsV1`). Applied backwards, step 05 first,
 * because every earlier step is parameterized by a successor's script hash —
 * and step 03 uniquely by TWO downstream hashes (step 04's reference-input
 * scan and step 05's direct close). Every step deploys as a reference script
 * in production per the standing reference-script ruling.
 */
export const buildMintAuthorizationChainV1 = ({
  realBlueprint,
  computationThreadPolicyId,
  fraudProofPolicyId,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
  hubOraclePolicyId,
}: {
  readonly realBlueprint: Blueprint;
  readonly computationThreadPolicyId: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofTokenAddressData: Data;
  readonly fieldPreimageCertificatePolicyId: string;
  readonly hubOraclePolicyId: string;
}): MintAuthorizationContractsV1["steps"] => {
  const step05 = makeSpendingValidator(
    applyCompiledScript(
      realBlueprint,
      MINT_AUTHORIZATION_BLUEPRINT_TITLES_V1.step05,
      [
        fraudProofPolicyId,
        fraudProofTokenAddressData,
        computationThreadPolicyId,
      ],
    ),
  );
  const step04 = makeSpendingValidator(
    applyCompiledScript(
      realBlueprint,
      MINT_AUTHORIZATION_BLUEPRINT_TITLES_V1.step04,
      [
        step05.spendingScriptHash,
        computationThreadPolicyId,
        fieldPreimageCertificatePolicyId,
      ],
    ),
  );
  const step03 = makeSpendingValidator(
    applyCompiledScript(
      realBlueprint,
      MINT_AUTHORIZATION_BLUEPRINT_TITLES_V1.step03,
      [
        step04.spendingScriptHash,
        step05.spendingScriptHash,
        computationThreadPolicyId,
        fieldPreimageCertificatePolicyId,
      ],
    ),
  );
  const step02 = makeSpendingValidator(
    applyCompiledScript(
      realBlueprint,
      MINT_AUTHORIZATION_BLUEPRINT_TITLES_V1.step02,
      [
        step03.spendingScriptHash,
        computationThreadPolicyId,
        fieldPreimageCertificatePolicyId,
      ],
    ),
  );
  const step01 = makeSpendingValidator(
    applyCompiledScript(
      realBlueprint,
      MINT_AUTHORIZATION_BLUEPRINT_TITLES_V1.step01,
      [step02.spendingScriptHash, computationThreadPolicyId, hubOraclePolicyId],
    ),
  );
  return [step01, step02, step03, step04, step05];
};

export const buildMinimalFaultProofContracts = async (
  realBlueprint: Blueprint,
  alwaysBlueprint: Blueprint,
  nonceUtxo: UTxO,
  {
    realNonExistentInput = false,
    realInvalidRange = false,
    realTransitionTrace = false,
    realZeroInput = false,
    realDaHashPreimage = false,
    realFabricatedDeposit = false,
    realFabricatedWithdrawal = false,
    realInputNoIdx = false,
    realNoReferenceInput = false,
    realReferenceInputNoIdx = false,
    realInvalidSignature = false,
    realValidationTraceDispute = false,
    realNativeScriptDecoding = false,
    realMissingSignature = false,
    realMissingNativeScriptTx = false,
    realMissingNativeScriptUtxo = false,
    realNativeScriptInvalid = false,
    realMinAda = false,
    realCanonicalDecodability = false,
    realCommittedFieldShape = false,
    realWithdrawnReferenceInput = false,
    realMinFee = false,
    realDoubleWithdraw = false,
    realCrossBlockDuplicateEvent = false,
    realL2TxMistag = false,
    realWithdrawnInput = false,
    realWithdrawalMistag = false,
    realInputSetUniqueness = false,
    realValueNotPreserved = false,
    realMintAuthorization = false,
    alwaysFraudProofCatalogue = false,
    alwaysStateQueue = false,
    referenceScriptAuthPolicyId,
  }: {
    readonly realNonExistentInput?: boolean;
    readonly realInvalidRange?: boolean;
    readonly realTransitionTrace?: boolean;
    readonly realZeroInput?: boolean;
    readonly realDaHashPreimage?: boolean;
    readonly realFabricatedDeposit?: boolean;
    readonly realFabricatedWithdrawal?: boolean;
    readonly realInputNoIdx?: boolean;
    readonly realNoReferenceInput?: boolean;
    readonly realReferenceInputNoIdx?: boolean;
    readonly realInvalidSignature?: boolean;
    readonly realValidationTraceDispute?: boolean;
    readonly realNativeScriptDecoding?: boolean;
    readonly realMissingSignature?: boolean;
    readonly realMissingNativeScriptTx?: boolean;
    readonly realMissingNativeScriptUtxo?: boolean;
    readonly realNativeScriptInvalid?: boolean;
    readonly realMinAda?: boolean;
    readonly realCanonicalDecodability?: boolean;
    readonly realCommittedFieldShape?: boolean;
    readonly realWithdrawnReferenceInput?: boolean;
    readonly realMinFee?: boolean;
    readonly realDoubleWithdraw?: boolean;
    readonly realCrossBlockDuplicateEvent?: boolean;
    readonly realL2TxMistag?: boolean;
    readonly realWithdrawnInput?: boolean;
    readonly realWithdrawalMistag?: boolean;
    readonly realInputSetUniqueness?: boolean;
    readonly realValueNotPreserved?: boolean;
    readonly realMintAuthorization?: boolean;
    readonly alwaysFraudProofCatalogue?: boolean;
    /**
     * Test-only admission bypass for faults whose malformed header is rejected
     * by the production state queue before the fault-proof family can observe
     * it. The family under test, computation-thread policies, fraud-proof
     * policy, and removal path remain their production implementations.
     */
    readonly alwaysStateQueue?: boolean;
    readonly referenceScriptAuthPolicyId?: string;
  } = {},
): Promise<
  MidgardValidators & {
    readonly computationThread: SdkMintingValidator;
    readonly fabricatedDeposit?: FabricatedDepositContractsV1;
    readonly fabricatedWithdrawal?: FabricatedWithdrawalContractsV1;
    readonly nativeScriptDecoding?: NativeScriptDecodingContractsV1;
    readonly missingSignature?: MissingSignatureContractsV1;
    readonly missingNativeScriptTx?: MissingNativeScriptTxContractsV1;
    readonly missingNativeScriptUtxo?: MissingNativeScriptUtxoContractsV1;
    readonly nativeScriptInvalid?: NativeScriptInvalidContractsV1;
    readonly minAda?: MinAdaContractsV1;
    readonly canonicalDecodability?: CanonicalDecodabilityContractsV1;
    readonly committedFieldShape?: CommittedFieldShapeContractsV1;
    readonly withdrawnReferenceInput?: WithdrawnReferenceInputContractsV1;
    readonly minFee?: MinFeeContractsV1;
    readonly doubleWithdraw?: DoubleWithdrawContractsV1;
    readonly crossBlockDuplicateEvent?: CrossBlockDuplicateEventContractsV1;
    readonly l2TxMistag?: L2TxMistagContractsV1;
    readonly withdrawnInput?: WithdrawnInputContractsV1;
    readonly withdrawalMistag?: WithdrawalMistagContractsV1;
    readonly inputSetUniqueness?: InputSetUniquenessContractsV1;
    readonly valueNotPreserved?: ValueNotPreservedContractsV1;
    readonly mintAuthorization?: MintAuthorizationContractsV1;
  }
> => {
  // This integration test proves the real active-operators slashing and
  // scheduler removal path. Registered/retired operator setup remains
  // scaffolded only where needed to support the focused removal flow.
  const base = makeAlwaysSucceedsContracts(alwaysBlueprint);
  const hubOracle = makeMintingValidator(
    applyCompiledScript(realBlueprint, "hub_oracle.mint.mint", [
      new Constr(0, [
        nonceUtxo.txHash.toLowerCase(),
        BigInt(nonceUtxo.outputIndex),
      ]),
      HUB_ORACLE_ASSET_NAME,
    ]),
  );
  const hubOracleAuth: AuthenticatedValidator = {
    ...hubOracle,
    spendingScriptCBOR: hubOracle.mintingScriptCBOR,
    spendingScript: hubOracle.mintingScript as SpendingValidator,
    spendingScriptHash: hubOracle.policyId,
    spendingScriptAddress: credentialToAddress(
      network,
      scriptHashToCredential(hubOracle.policyId),
    ),
  };
  const withHubOracle = {
    ...base,
    hubOracle: hubOracleAuth,
  };

  const fraudProofCatalogue = alwaysFraudProofCatalogue
    ? withHubOracle.fraudProofCatalogue
    : makeAuthenticatedValidator(
        applyCompiledScript(realBlueprint, "fraud_proof_catalogue.mint.mint", [
          hubOracle.policyId,
        ]),
        getCompiledScript(realBlueprint, "fraud_proof_catalogue.spend.else"),
      );
  const withCatalogue = {
    ...withHubOracle,
    fraudProofCatalogue,
  };

  const retiredOperatorsMinting = makeMintingValidator(
    applyCompiledScript(
      realBlueprint,
      "operator_directory/retired_operators.mint.mint",
      [hubOracle.policyId],
    ),
  );
  const retiredOperators: AuthenticatedValidator = {
    ...retiredOperatorsMinting,
    ...makeSpendingValidator(
      applyCompiledScript(
        realBlueprint,
        "operator_directory/retired_operators.spend.spend",
        [retiredOperatorsMinting.policyId],
      ),
    ),
  };
  const withRetiredOperators = {
    ...withCatalogue,
    retiredOperators,
  };

  const registeredOperatorsMinting = makeMintingValidator(
    applyCompiledScript(
      realBlueprint,
      "operator_directory/registered_operators.mint.mint",
      [retiredOperators.policyId, hubOracle.policyId],
    ),
  );
  const registeredOperators: AuthenticatedValidator = {
    ...registeredOperatorsMinting,
    ...makeSpendingValidator(
      applyCompiledScript(
        realBlueprint,
        "operator_directory/registered_operators.spend.spend",
        [registeredOperatorsMinting.policyId],
      ),
    ),
  };
  const withRegisteredOperators = {
    ...withRetiredOperators,
    registeredOperators,
  };

  const activeOperatorsMinting = makeMintingValidator(
    applyCompiledScript(
      realBlueprint,
      "operator_directory/active_operators.mint.mint",
      [
        hubOracle.policyId,
        registeredOperators.policyId,
        retiredOperators.policyId,
      ],
    ),
  );
  const activeOperators: AuthenticatedValidator = {
    ...activeOperatorsMinting,
    ...makeSpendingValidator(
      applyCompiledScript(
        realBlueprint,
        "operator_directory/active_operators.spend.spend",
        [activeOperatorsMinting.policyId, hubOracle.policyId],
      ),
    ),
  };
  const withActiveOperators = {
    ...withRegisteredOperators,
    activeOperators,
  };

  const doubleSpendContracts = await Effect.runPromise(
    buildDoubleSpendFaultProofContracts({
      blueprint: parseFaultProofBlueprint(realBlueprint),
      network,
      hubOraclePolicyId: hubOracle.policyId,
      fraudProofCataloguePolicyId: fraudProofCatalogue.policyId,
    }),
  );
  // Every real family must share the double-spend family's fraud-proof policy:
  // the catalogue, the computation-thread mints, and removal all key on that
  // one policy, so a family compiling to a different policy id is a
  // parameterization bug.
  const buildFamilyContracts = async <
    T extends { readonly fraudProof: { readonly policyId: string } },
    E,
  >(
    enabled: boolean,
    build: (args: {
      readonly blueprint: ReturnType<typeof parseFaultProofBlueprint>;
      readonly network: typeof network;
      readonly hubOraclePolicyId: string;
      readonly fraudProofCataloguePolicyId: string;
      readonly referenceScriptAuthPolicyId: string;
    }) => Effect.Effect<T, E>,
  ): Promise<T | undefined> => {
    if (!enabled) {
      return undefined;
    }
    const familyContracts = await Effect.runPromise(
      build({
        blueprint: parseFaultProofBlueprint(cloneBlueprint(realBlueprint)),
        network,
        hubOraclePolicyId: hubOracle.policyId,
        fraudProofCataloguePolicyId: fraudProofCatalogue.policyId,
        referenceScriptAuthPolicyId:
          referenceScriptAuthPolicyId ?? base.referenceScriptAuth.policyId,
      }),
    );
    expect(familyContracts.fraudProof.policyId).toBe(
      doubleSpendContracts.fraudProof.policyId,
    );
    return familyContracts;
  };
  const nonExistentInputContracts = await buildFamilyContracts(
    realNonExistentInput,
    buildNonExistentInputFaultProofContracts,
  );
  const invalidRangeContracts = await buildFamilyContracts(
    realInvalidRange,
    buildInvalidRangeFaultProofContracts,
  );
  const transitionTraceContracts = await buildFamilyContracts(
    realTransitionTrace,
    buildTransitionTraceFaultProofContracts,
  );
  const validationTraceDisputeContracts = await buildFamilyContracts(
    realValidationTraceDispute,
    buildValidationTraceDisputeFaultProofContracts,
  );
  const zeroInputContracts = await buildFamilyContracts(
    realZeroInput,
    buildZeroInputFaultProofContracts,
  );
  const daHashPreimageContracts = await buildFamilyContracts(
    realDaHashPreimage,
    buildDaHashPreimageFaultProofContracts,
  );
  const fabricatedDepositContracts = await buildFamilyContracts(
    realFabricatedDeposit,
    buildFabricatedDepositFaultProofContracts,
  );
  const fabricatedWithdrawalContracts = await buildFamilyContracts(
    realFabricatedWithdrawal,
    buildFabricatedWithdrawalFaultProofContracts,
  );
  const inputNoIdxContracts = await buildFamilyContracts(
    realInputNoIdx,
    buildInputNoIdxFaultProofContracts,
  );
  const noReferenceInputContracts = await buildFamilyContracts(
    realNoReferenceInput,
    buildNoReferenceInputFaultProofContracts,
  );
  const referenceInputNoIdxContracts = await buildFamilyContracts(
    realReferenceInputNoIdx,
    buildReferenceInputNoIdxFaultProofContracts,
  );
  const invalidSignatureContracts = await buildFamilyContracts(
    realInvalidSignature,
    buildInvalidSignatureFaultProofContracts,
  );
  const minFeeContracts = await buildFamilyContracts(
    realMinFee,
    buildMinFeeFaultProofContracts,
  );
  const nativeScriptDecodingContracts = await buildFamilyContracts(
    realNativeScriptDecoding,
    buildNativeScriptDecodingFaultProofContracts,
  );
  const missingSignatureContracts = await buildFamilyContracts(
    realMissingSignature,
    buildMissingSignatureFaultProofContracts,
  );
  const missingNativeScriptTxContracts = await buildFamilyContracts(
    realMissingNativeScriptTx,
    buildMissingNativeScriptTxFaultProofContracts,
  );
  const missingNativeScriptUtxoContracts = await buildFamilyContracts(
    realMissingNativeScriptUtxo,
    buildMissingNativeScriptUtxoFaultProofContracts,
  );
  const nativeScriptInvalidContracts = await buildFamilyContracts(
    realNativeScriptInvalid,
    buildNativeScriptInvalidFaultProofContracts,
  );
  const minAdaContracts = await buildFamilyContracts(
    realMinAda,
    buildMinAdaFaultProofContracts,
  );
  const withdrawnReferenceInputContracts = await buildFamilyContracts(
    realWithdrawnReferenceInput,
    buildWithdrawnReferenceInputFaultProofContracts,
  );
  const canonicalDecodabilityContracts = await buildFamilyContracts(
    realCanonicalDecodability,
    buildCanonicalDecodabilityFaultProofContracts,
  );
  const committedFieldShapeContracts = await buildFamilyContracts(
    realCommittedFieldShape,
    buildCommittedFieldShapeFaultProofContracts,
  );
  const withdrawnInputContracts = await buildFamilyContracts(
    realWithdrawnInput,
    buildWithdrawnInputFaultProofContracts,
  );
  const withdrawalMistagContracts = await buildFamilyContracts(
    realWithdrawalMistag,
    buildWithdrawalMistagFaultProofContracts,
  );
  const doubleWithdrawContracts = await buildFamilyContracts(
    realDoubleWithdraw,
    buildDoubleWithdrawFaultProofContracts,
  );
  const crossBlockDuplicateEventContracts = await buildFamilyContracts(
    realCrossBlockDuplicateEvent,
    buildCrossBlockDuplicateEventFaultProofContracts,
  );
  const l2TxMistagContracts = await buildFamilyContracts(
    realL2TxMistag,
    buildL2TxMistagFaultProofContracts,
  );
  const valueNotPreservedContracts = await buildFamilyContracts(
    realValueNotPreserved,
    buildValueNotPreservedFaultProofContracts,
  );
  const inputSetUniquenessContracts = await buildFamilyContracts(
    realInputSetUniqueness,
    buildInputSetUniquenessFaultProofContracts,
  );
  const mintAuthorizationContracts = await buildFamilyContracts(
    realMintAuthorization,
    buildMintAuthorizationFaultProofContracts,
  );
  const activeOperatorsAddressData = await Effect.runPromise(
    addressDataFromBech32(
      withActiveOperators.activeOperators.spendingScriptAddress,
    ).pipe(
      Effect.map((addressData) => Data.from(Data.to(addressData, AddressData))),
    ),
  );
  const schedulerMinting = makeMintingValidator(
    applyCompiledScript(realBlueprint, "scheduler.mint.mint", [
      hubOracle.policyId,
    ]),
  );
  const scheduler: AuthenticatedValidator = {
    ...schedulerMinting,
    ...makeSpendingValidator(
      applyCompiledScript(realBlueprint, "scheduler.spend.spend", [
        withActiveOperators.registeredOperators.policyId,
        activeOperatorsAddressData,
        withActiveOperators.activeOperators.policyId,
        schedulerMinting.policyId,
        hubOracle.policyId,
      ]),
    ),
  };
  const withScheduler = {
    ...withActiveOperators,
    scheduler,
  };
  // The availability-challenge policy is a stand-in here, mirroring the sdk
  // state-queue harness: these emulator flows never open a challenge, the
  // parameter only has to be a distinct policy id.
  const availabilityChallengePolicyId = base.escapeHatch.policyId;
  const correctionLock = makeSpendingValidator(
    applyCompiledScript(realBlueprint, "correction_lock.spend.spend", [
      hubOracle.policyId,
      availabilityChallengePolicyId,
    ]),
  );
  const stateQueueMinting = makeMintingValidator(
    applyCompiledScript(realBlueprint, "state_queue.mint.mint", [
      hubOracle.policyId,
      correctionLock.spendingScriptHash,
      withScheduler.activeOperators.policyId,
      activeOperatorsAddressData,
      withScheduler.retiredOperators.policyId,
      withScheduler.scheduler.policyId,
      doubleSpendContracts.fraudProof.policyId,
      withScheduler.settlement.policyId,
      withScheduler.daAttestation.policyId,
      availabilityChallengePolicyId,
      referenceScriptAuthPolicyId ?? base.referenceScriptAuth.policyId,
    ]),
  );
  const stateQueueSpending = makeSpendingValidator(
    applyCompiledScript(realBlueprint, "state_queue.spend.spend", [
      stateQueueMinting.policyId,
      withScheduler.daAttestation.policyId,
      availabilityChallengePolicyId,
    ]),
  );
  const stateQueueYields = {
    commit: makeWithdrawalValidator(
      applyCompiledScript(realBlueprint, "state_queue_yields.commit.withdraw", [
        stateQueueMinting.policyId,
        hubOracle.policyId,
        correctionLock.spendingScriptHash,
        withScheduler.activeOperators.policyId,
        activeOperatorsAddressData,
        withScheduler.scheduler.policyId,
        withScheduler.daAttestation.policyId,
      ]),
    ),
    unattestedTimeout: makeWithdrawalValidator(
      applyCompiledScript(
        realBlueprint,
        "state_queue_yields.remove_unattested.withdraw",
        [
          stateQueueMinting.policyId,
          hubOracle.policyId,
          correctionLock.spendingScriptHash,
        ],
      ),
    ),
    unavailableTimeout: makeWithdrawalValidator(
      applyCompiledScript(
        realBlueprint,
        "state_queue_yields.remove_unavailable.withdraw",
        [
          stateQueueMinting.policyId,
          hubOracle.policyId,
          correctionLock.spendingScriptHash,
          availabilityChallengePolicyId,
        ],
      ),
    ),
    fraudRemoval: makeWithdrawalValidator(
      applyCompiledScript(
        realBlueprint,
        "state_queue_yields.remove_fraudulent.withdraw",
        [
          stateQueueMinting.policyId,
          hubOracle.policyId,
          correctionLock.spendingScriptHash,
          withScheduler.activeOperators.policyId,
          withScheduler.retiredOperators.policyId,
          doubleSpendContracts.fraudProof.policyId,
        ],
      ),
    ),
    merge: makeWithdrawalValidator(
      applyCompiledScript(realBlueprint, "state_queue_yields.merge.withdraw", [
        stateQueueMinting.policyId,
        hubOracle.policyId,
        correctionLock.spendingScriptHash,
        withScheduler.settlement.policyId,
        withScheduler.daAttestation.policyId,
      ]),
    ),
  };
  const stateQueue = alwaysStateQueue
    ? (() => {
        const isolated = makeIsolatedAlwaysSucceedsAuthenticatedValidator();
        const yieldValidator = makeWithdrawalValidator(
          isolated.mintingScriptCBOR,
        );
        return {
          ...isolated,
          yields: {
            commit: yieldValidator,
            unattestedTimeout: yieldValidator,
            unavailableTimeout: yieldValidator,
            fraudRemoval: yieldValidator,
            merge: yieldValidator,
          },
        };
      })()
    : {
        ...stateQueueMinting,
        ...stateQueueSpending,
        yields: stateQueueYields,
      };

  // The Q39/Q40 submitters take an explicit focused contracts record. Assemble
  // it from the same parameterized chains whose step-01 hashes occupy their
  // canonical production catalogue categories.
  const fabricatedDeposit: FabricatedDepositContractsV1 | undefined =
    fabricatedDepositContracts === undefined
      ? undefined
      : {
          steps: fabricatedDepositContracts.fabricatedDeposit.steps,
          computationThread: fabricatedDepositContracts.computationThread,
          fraudProof: fabricatedDepositContracts.fraudProof,
          hubOraclePolicyId: hubOracle.policyId,
          stateQueuePolicyId: stateQueue.policyId,
          categoryId: FABRICATED_DEPOSIT_FRAUD_CATEGORY_ID_V1,
        };
  const fieldPreimageCertificateMinting = makeMintingValidator(
    getCompiledScript(
      realBlueprint,
      FAULT_PROOF_SHARED_TITLES.fieldPreimageCertificateMint,
    ),
  );
  const fieldPreimageCertificatePolicyId =
    fieldPreimageCertificateMinting.policyId;

  // Family adapters retain their focused test-facing records, but the scripts
  // now come from the same canonical SDK builders used for production
  // deployment. This keeps catalogue and removal hashes identical without
  // reintroducing pre-registration sidecars.
  const nativeScriptDecoding: NativeScriptDecodingContractsV1 | undefined =
    nativeScriptDecodingContracts === undefined
      ? undefined
      : {
          steps: nativeScriptDecodingContracts.nativeScriptDecoding.steps,
          computationThread: nativeScriptDecodingContracts.computationThread,
          fraudProof: nativeScriptDecodingContracts.fraudProof,
          hubOraclePolicyId: hubOracle.policyId,
          stateQueuePolicyId: stateQueue.policyId,
          fieldPreimageCertificatePolicyId,
        };
  const missingSignature: MissingSignatureContractsV1 | undefined =
    missingSignatureContracts === undefined
      ? undefined
      : {
          steps: missingSignatureContracts.missingSignature.steps,
          computationThread: missingSignatureContracts.computationThread,
          fraudProof: missingSignatureContracts.fraudProof,
          hubOraclePolicyId: hubOracle.policyId,
          stateQueuePolicyId: stateQueueMinting.policyId,
          fieldPreimageCertificatePolicyId,
        };
  const missingNativeScriptTx: MissingNativeScriptTxContractsV1 | undefined =
    missingNativeScriptTxContracts === undefined
      ? undefined
      : {
          steps: missingNativeScriptTxContracts.missingNativeScriptTx.steps,
          computationThread: missingNativeScriptTxContracts.computationThread,
          fraudProof: missingNativeScriptTxContracts.fraudProof,
          hubOraclePolicyId: hubOracle.policyId,
          stateQueuePolicyId: stateQueueMinting.policyId,
          fieldPreimageCertificatePolicyId,
        };
  const missingNativeScriptUtxo:
    | MissingNativeScriptUtxoContractsV1
    | undefined =
    missingNativeScriptUtxoContracts === undefined
      ? undefined
      : {
          steps: missingNativeScriptUtxoContracts.missingNativeScriptUtxo.steps,
          computationThread: missingNativeScriptUtxoContracts.computationThread,
          fraudProof: missingNativeScriptUtxoContracts.fraudProof,
          hubOraclePolicyId: hubOracle.policyId,
          stateQueuePolicyId: stateQueueMinting.policyId,
          fieldPreimageCertificatePolicyId,
        };
  const nativeScriptInvalid: NativeScriptInvalidContractsV1 | undefined =
    nativeScriptInvalidContracts === undefined
      ? undefined
      : {
          steps: nativeScriptInvalidContracts.nativeScriptInvalid.steps,
          computationThread: nativeScriptInvalidContracts.computationThread,
          fraudProof: nativeScriptInvalidContracts.fraudProof,
          hubOraclePolicyId: hubOracle.policyId,
          stateQueuePolicyId: stateQueueMinting.policyId,
          fieldPreimageCertificatePolicyId,
        };
  const minAda: MinAdaContractsV1 | undefined =
    minAdaContracts === undefined
      ? undefined
      : {
          steps: minAdaContracts.minAda.steps,
          yields: minAdaContracts.minAda.yields,
          computationThread: minAdaContracts.computationThread,
          fraudProof: minAdaContracts.fraudProof,
          hubOraclePolicyId: hubOracle.policyId,
          stateQueuePolicyId: stateQueueMinting.policyId,
          fieldPreimageCertificatePolicyId,
          referenceScriptAuthPolicyId:
            referenceScriptAuthPolicyId ?? base.referenceScriptAuth.policyId,
        };
  const canonicalDecodability: CanonicalDecodabilityContractsV1 | undefined =
    canonicalDecodabilityContracts === undefined
      ? undefined
      : {
          steps: canonicalDecodabilityContracts.canonicalDecodability.steps,
          computationThread: canonicalDecodabilityContracts.computationThread,
          fraudProof: canonicalDecodabilityContracts.fraudProof,
          hubOraclePolicyId: hubOracle.policyId,
          stateQueuePolicyId: stateQueueMinting.policyId,
          fieldPreimageCertificatePolicyId,
        };
  const committedFieldShape: CommittedFieldShapeContractsV1 | undefined =
    committedFieldShapeContracts === undefined
      ? undefined
      : {
          steps: committedFieldShapeContracts.committedFieldShape.steps,
          computationThread: committedFieldShapeContracts.computationThread,
          fraudProof: committedFieldShapeContracts.fraudProof,
          hubOraclePolicyId: hubOracle.policyId,
          stateQueuePolicyId: stateQueueMinting.policyId,
          fieldPreimageCertificatePolicyId,
        };
  const withdrawnReferenceInput:
    | WithdrawnReferenceInputContractsV1
    | undefined =
    withdrawnReferenceInputContracts === undefined
      ? undefined
      : {
          steps: withdrawnReferenceInputContracts.withdrawnReferenceInput.steps,
          computationThread: withdrawnReferenceInputContracts.computationThread,
          fraudProof: withdrawnReferenceInputContracts.fraudProof,
          hubOraclePolicyId: hubOracle.policyId,
          stateQueuePolicyId: stateQueueMinting.policyId,
          fieldPreimageCertificatePolicyId,
        };
  const minFee: MinFeeContractsV1 | undefined = realMinFee
    ? {
        steps: minFeeContracts!.minFee.steps,
        computationThread: minFeeContracts!.computationThread,
        fraudProof: minFeeContracts!.fraudProof,
        hubOraclePolicyId: hubOracle.policyId,
        stateQueuePolicyId: stateQueueMinting.policyId,
        fieldPreimageCertificatePolicyId,
      }
    : undefined;
  const doubleWithdraw: DoubleWithdrawContractsV1 | undefined =
    doubleWithdrawContracts === undefined
      ? undefined
      : {
          steps: doubleWithdrawContracts.doubleWithdraw.steps,
          computationThread: doubleWithdrawContracts.computationThread,
          fraudProof: doubleWithdrawContracts.fraudProof,
          hubOraclePolicyId: hubOracle.policyId,
          stateQueuePolicyId: stateQueueMinting.policyId,
        };
  const crossBlockDuplicateEvent:
    | CrossBlockDuplicateEventContractsV1
    | undefined =
    crossBlockDuplicateEventContracts === undefined
      ? undefined
      : {
          steps:
            crossBlockDuplicateEventContracts.crossBlockDuplicateEvent.steps,
          computationThread:
            crossBlockDuplicateEventContracts.computationThread,
          fraudProof: crossBlockDuplicateEventContracts.fraudProof,
          hubOraclePolicyId: hubOracle.policyId,
          stateQueuePolicyId: stateQueueMinting.policyId,
        };
  const l2TxMistag: L2TxMistagContractsV1 | undefined =
    l2TxMistagContracts === undefined
      ? undefined
      : {
          steps: l2TxMistagContracts.l2TxMistag.steps,
          computationThread: l2TxMistagContracts.computationThread,
          fraudProof: l2TxMistagContracts.fraudProof,
          hubOraclePolicyId: hubOracle.policyId,
          stateQueuePolicyId: stateQueueMinting.policyId,
        };
  const withdrawnInput: WithdrawnInputContractsV1 | undefined =
    withdrawnInputContracts === undefined
      ? undefined
      : {
          steps: withdrawnInputContracts.withdrawnInput.steps,
          computationThread: withdrawnInputContracts.computationThread,
          fraudProof: withdrawnInputContracts.fraudProof,
          hubOraclePolicyId: hubOracle.policyId,
          stateQueuePolicyId: stateQueueMinting.policyId,
          fieldPreimageCertificatePolicyId,
        };
  const withdrawalMistag: WithdrawalMistagContractsV1 | undefined =
    withdrawalMistagContracts === undefined
      ? undefined
      : {
          steps: withdrawalMistagContracts.withdrawalMistag.steps,
          computationThread: withdrawalMistagContracts.computationThread,
          fraudProof: withdrawalMistagContracts.fraudProof,
          hubOraclePolicyId: hubOracle.policyId,
          stateQueuePolicyId: stateQueueMinting.policyId,
        };
  const valueNotPreserved: ValueNotPreservedContractsV1 | undefined =
    valueNotPreservedContracts === undefined
      ? undefined
      : {
          steps: valueNotPreservedContracts.valueNotPreserved.steps,
          computationThread: valueNotPreservedContracts.computationThread,
          fraudProof: valueNotPreservedContracts.fraudProof,
          hubOraclePolicyId: hubOracle.policyId,
          stateQueuePolicyId: stateQueueMinting.policyId,
          fieldPreimageCertificatePolicyId,
        };
  const inputSetUniqueness: InputSetUniquenessContractsV1 | undefined =
    inputSetUniquenessContracts === undefined
      ? undefined
      : {
          steps: inputSetUniquenessContracts.inputSetUniqueness.steps,
          computationThread: inputSetUniquenessContracts.computationThread,
          fraudProof: inputSetUniquenessContracts.fraudProof,
          hubOraclePolicyId: hubOracle.policyId,
          stateQueuePolicyId: stateQueueMinting.policyId,
          fieldPreimageCertificatePolicyId,
        };
  const mintAuthorization: MintAuthorizationContractsV1 | undefined =
    mintAuthorizationContracts === undefined
      ? undefined
      : {
          steps: mintAuthorizationContracts.mintAuthorization.steps,
          computationThread: mintAuthorizationContracts.computationThread,
          fraudProof: mintAuthorizationContracts.fraudProof,
          hubOraclePolicyId: hubOracle.policyId,
          stateQueuePolicyId: stateQueueMinting.policyId,
          fieldPreimageCertificatePolicyId,
        };
  const fabricatedWithdrawal: FabricatedWithdrawalContractsV1 | undefined =
    fabricatedWithdrawalContracts === undefined
      ? undefined
      : {
          steps: fabricatedWithdrawalContracts.fabricatedWithdrawal.steps,
          computationThread: fabricatedWithdrawalContracts.computationThread,
          fraudProof: fabricatedWithdrawalContracts.fraudProof,
          hubOraclePolicyId: hubOracle.policyId,
          stateQueuePolicyId: stateQueue.policyId,
          categoryId: FABRICATED_WITHDRAWAL_FRAUD_CATEGORY_ID_V1,
        };
  const fraudProofContracts: FaultProofContractChains = {
    ...withActiveOperators.fraudProofContracts,
    doubleSpend: doubleSpendContracts.doubleSpend,
    nonExistentInput:
      nonExistentInputContracts?.nonExistentInput ??
      withActiveOperators.fraudProofContracts.nonExistentInput,
    nonExistentInputNoIndex:
      inputNoIdxContracts?.nonExistentInputNoIndex ??
      withActiveOperators.fraudProofContracts.nonExistentInputNoIndex,
    invalidRange:
      invalidRangeContracts?.invalidRange ??
      withActiveOperators.fraudProofContracts.invalidRange,
    transitionTrace:
      transitionTraceContracts?.transitionTrace ??
      withActiveOperators.fraudProofContracts.transitionTrace,
    zeroInput:
      zeroInputContracts?.zeroInput ??
      withActiveOperators.fraudProofContracts.zeroInput,
    validationTraceDispute:
      validationTraceDisputeContracts?.validationTraceDispute ??
      withActiveOperators.fraudProofContracts.validationTraceDispute,
    daHashPreimage:
      daHashPreimageContracts?.daHashPreimage ??
      withActiveOperators.fraudProofContracts.daHashPreimage,
    noReferenceInput:
      noReferenceInputContracts?.noReferenceInput ??
      withActiveOperators.fraudProofContracts.noReferenceInput,
    referenceInputNoIdx:
      referenceInputNoIdxContracts?.referenceInputNoIdx ??
      withActiveOperators.fraudProofContracts.referenceInputNoIdx,
    invalidSignature:
      invalidSignatureContracts?.invalidSignature ??
      withActiveOperators.fraudProofContracts.invalidSignature,
    fabricatedDeposit:
      fabricatedDeposit === undefined
        ? withActiveOperators.fraudProofContracts.fabricatedDeposit
        : chainFromSteps(fabricatedDeposit.steps),
    fabricatedWithdrawal:
      fabricatedWithdrawal === undefined
        ? withActiveOperators.fraudProofContracts.fabricatedWithdrawal
        : chainFromSteps(fabricatedWithdrawal.steps),
    nativeScriptDecoding:
      nativeScriptDecoding === undefined
        ? withActiveOperators.fraudProofContracts.nativeScriptDecoding
        : chainFromSteps(nativeScriptDecoding.steps),
    missingSignature:
      missingSignature === undefined
        ? withActiveOperators.fraudProofContracts.missingSignature
        : chainFromSteps(missingSignature.steps),
    missingNativeScriptTx:
      missingNativeScriptTx === undefined
        ? withActiveOperators.fraudProofContracts.missingNativeScriptTx
        : chainFromSteps(missingNativeScriptTx.steps),
    missingNativeScriptUtxo:
      missingNativeScriptUtxo === undefined
        ? withActiveOperators.fraudProofContracts.missingNativeScriptUtxo
        : chainFromSteps(missingNativeScriptUtxo.steps),
    nativeScriptInvalid:
      nativeScriptInvalid === undefined
        ? withActiveOperators.fraudProofContracts.nativeScriptInvalid
        : chainFromSteps(nativeScriptInvalid.steps),
    minAda:
      minAda === undefined
        ? withActiveOperators.fraudProofContracts.minAda
        : { ...chainFromSteps(minAda.steps), yields: minAda.yields },
    withdrawnReferenceInput:
      withdrawnReferenceInput === undefined
        ? withActiveOperators.fraudProofContracts.withdrawnReferenceInput
        : chainFromSteps(withdrawnReferenceInput.steps),
    canonicalDecodability:
      canonicalDecodability === undefined
        ? withActiveOperators.fraudProofContracts.canonicalDecodability
        : chainFromSteps(canonicalDecodability.steps),
    committedFieldShape:
      committedFieldShape === undefined
        ? withActiveOperators.fraudProofContracts.committedFieldShape
        : chainFromSteps(committedFieldShape.steps),
    minFee:
      minFee === undefined
        ? withActiveOperators.fraudProofContracts.minFee
        : chainFromSteps(minFee.steps),
    withdrawalMistag:
      withdrawalMistag === undefined
        ? withActiveOperators.fraudProofContracts.withdrawalMistag
        : chainFromSteps(withdrawalMistag.steps),
    doubleWithdraw:
      doubleWithdraw === undefined
        ? withActiveOperators.fraudProofContracts.doubleWithdraw
        : chainFromSteps(doubleWithdraw.steps),
    crossBlockDuplicateEvent:
      crossBlockDuplicateEvent === undefined
        ? withActiveOperators.fraudProofContracts.crossBlockDuplicateEvent
        : chainFromSteps(crossBlockDuplicateEvent.steps),
    l2TxMistag:
      l2TxMistag === undefined
        ? withActiveOperators.fraudProofContracts.l2TxMistag
        : chainFromSteps(l2TxMistag.steps),
    withdrawnInput:
      withdrawnInput === undefined
        ? withActiveOperators.fraudProofContracts.withdrawnInput
        : chainFromSteps(withdrawnInput.steps),
    valueNotPreserved:
      valueNotPreserved === undefined
        ? withActiveOperators.fraudProofContracts.valueNotPreserved
        : chainFromSteps(valueNotPreserved.steps),
    inputSetUniqueness:
      inputSetUniqueness === undefined
        ? withActiveOperators.fraudProofContracts.inputSetUniqueness
        : chainFromSteps(inputSetUniqueness.steps),
    mintAuthorization:
      mintAuthorization === undefined
        ? withActiveOperators.fraudProofContracts.mintAuthorization
        : chainFromSteps(mintAuthorization.steps),
  };

  return {
    ...withScheduler,
    ...(fabricatedDeposit === undefined ? {} : { fabricatedDeposit }),
    ...(fabricatedWithdrawal === undefined ? {} : { fabricatedWithdrawal }),
    ...(nativeScriptDecoding === undefined ? {} : { nativeScriptDecoding }),
    ...(missingSignature === undefined ? {} : { missingSignature }),
    ...(missingNativeScriptTx === undefined ? {} : { missingNativeScriptTx }),
    ...(missingNativeScriptUtxo === undefined
      ? {}
      : { missingNativeScriptUtxo }),
    ...(nativeScriptInvalid === undefined ? {} : { nativeScriptInvalid }),
    ...(minAda === undefined ? {} : { minAda }),
    ...(canonicalDecodability === undefined ? {} : { canonicalDecodability }),
    ...(committedFieldShape === undefined ? {} : { committedFieldShape }),
    ...(withdrawnReferenceInput === undefined
      ? {}
      : { withdrawnReferenceInput }),
    ...(minFee === undefined ? {} : { minFee }),
    ...(doubleWithdraw === undefined ? {} : { doubleWithdraw }),
    ...(crossBlockDuplicateEvent === undefined
      ? {}
      : { crossBlockDuplicateEvent }),
    ...(l2TxMistag === undefined ? {} : { l2TxMistag }),
    ...(withdrawnInput === undefined ? {} : { withdrawnInput }),
    ...(withdrawalMistag === undefined ? {} : { withdrawalMistag }),
    ...(inputSetUniqueness === undefined ? {} : { inputSetUniqueness }),
    ...(valueNotPreserved === undefined ? {} : { valueNotPreserved }),
    ...(mintAuthorization === undefined ? {} : { mintAuthorization }),
    cekProgramMaterial:
      validationTraceDisputeContracts?.validationTraceDispute
        .cekProgramMaterial ?? withScheduler.cekProgramMaterial,
    // Certification must mint and park the output under the same canonical
    // dual-purpose script every SDK family step is parameterized with.
    fieldPreimageCertificate: {
      ...fieldPreimageCertificateMinting,
      ...makeSpendingValidator(
        fieldPreimageCertificateMinting.mintingScriptCBOR,
      ),
    },
    correctionLock,
    stateQueue,
    // Canonical computation-thread minting validator shared by every family
    // (each family chain is parameterized with the same dual-purpose script).
    computationThread: doubleSpendContracts.computationThread,
    fraudProof: {
      ...doubleSpendContracts.fraudProof,
      policyId: doubleSpendContracts.fraudProof.policyId,
      mintingScript: doubleSpendContracts.fraudProof.mintingScript,
      mintingScriptCBOR: doubleSpendContracts.fraudProof.mintingScriptCBOR,
    },
    fraudProofContracts,
    fraudProofs: fraudProofContractsToFirstSteps(fraudProofContracts),
  };
};
