import {
  fraudProofContractsToFirstSteps,
  type MidgardValidators,
} from "@al-ft/midgard-sdk";

import { type Blueprint } from "./blueprints.js";
import { makeAlwaysSucceedsFaultProofContracts } from "./validators.make-always-succeeds-fault-proof-contracts.js";
import {
  alwaysAuthenticated,
  alwaysScript,
  makeMintingValidator,
  makeSpendingValidator,
  makeWithdrawalValidator,
} from "./validators.make-isolated-always-succeeds-authenticated-validator.js";

export const makeAlwaysSucceedsContracts = (
  blueprint: Blueprint,
): MidgardValidators => {
  const reserve = {
    ...makeSpendingValidator(
      alwaysScript(blueprint, "midgard", "reserve", "spend"),
    ),
    ...makeWithdrawalValidator(
      alwaysScript(blueprint, "midgard", "reserve", "withdraw"),
    ),
  };
  const fraudProofContracts = makeAlwaysSucceedsFaultProofContracts(
    blueprint,
    reserve,
  );
  const fraudProofs = fraudProofContractsToFirstSteps(fraudProofContracts);
  // The always-succeeds state-queue pair stands in for the script-decode one,
  // and its script for each class staker.
  const scriptDecodePair = alwaysAuthenticated(blueprint, "state_queue");
  const scriptDecodeStaker = makeWithdrawalValidator(
    scriptDecodePair.spendingScriptCBOR,
  );
  const scriptDecode = {
    ...scriptDecodePair,
    stakers: {
      term: scriptDecodeStaker,
      value: scriptDecodeStaker,
      list: scriptDecodeStaker,
      fold: scriptDecodeStaker,
      bytes: scriptDecodeStaker,
      data: scriptDecodeStaker,
    },
  };
  const fieldPreimage = makeSpendingValidator(
    alwaysScript(blueprint, "midgard", "state_queue", "spend"),
  );
  // #579 ruling A: this always-succeeds spend+mint pair used to stand in for the
  // retired `tx_field_receipt_v1` family. It now stands in for the §8.6
  // field-preimage certificate, which is the role the emulator set actually has
  // to fill — the tx-order mint is parameterized by the certificate policy id.
  const fieldPreimageCertificateV1 = {
    ...fieldPreimage,
    ...makeMintingValidator(
      alwaysScript(blueprint, "midgard", "state_queue", "mint"),
    ),
  };

  return {
    referenceScriptAuth: makeMintingValidator(
      alwaysScript(blueprint, "midgard", "state_queue", "mint"),
    ),
    hubOracle: {
      ...makeMintingValidator(
        alwaysScript(blueprint, "midgard", "hub_oracle", "mint"),
      ),
      ...makeSpendingValidator(
        alwaysScript(blueprint, "midgard", "hub_oracle", "mint"),
      ),
    },
    daParamsGovernor: alwaysAuthenticated(blueprint, "state_queue"),
    daAttestation: alwaysAuthenticated(blueprint, "state_queue"),
    daBondPool: alwaysAuthenticated(blueprint, "state_queue"),
    availabilityChallenge: {
      ...alwaysAuthenticated(blueprint, "state_queue"),
      yields: {
        open: makeWithdrawalValidator(
          alwaysScript(blueprint, "midgard", "state_queue", "mint"),
        ),
        settle: makeWithdrawalValidator(
          alwaysScript(blueprint, "midgard", "state_queue", "mint"),
        ),
        close: makeWithdrawalValidator(
          alwaysScript(blueprint, "midgard", "state_queue", "mint"),
        ),
        timeout: makeWithdrawalValidator(
          alwaysScript(blueprint, "midgard", "state_queue", "mint"),
        ),
      },
    },
    correctionLock: makeSpendingValidator(
      alwaysScript(blueprint, "midgard", "state_queue", "spend"),
    ),
    stateQueue: {
      ...alwaysAuthenticated(blueprint, "state_queue"),
      yields: {
        commit: makeWithdrawalValidator(
          alwaysScript(blueprint, "midgard", "state_queue", "mint"),
        ),
        unattestedTimeout: makeWithdrawalValidator(
          alwaysScript(blueprint, "midgard", "state_queue", "mint"),
        ),
        unavailableTimeout: makeWithdrawalValidator(
          alwaysScript(blueprint, "midgard", "state_queue", "mint"),
        ),
        fraudRemoval: makeWithdrawalValidator(
          alwaysScript(blueprint, "midgard", "state_queue", "mint"),
        ),
        merge: makeWithdrawalValidator(
          alwaysScript(blueprint, "midgard", "state_queue", "mint"),
        ),
      },
    },
    scheduler: alwaysAuthenticated(blueprint, "scheduler"),
    registeredOperators: alwaysAuthenticated(blueprint, "registered_operators"),
    activeOperators: alwaysAuthenticated(blueprint, "active_operators"),
    retiredOperators: alwaysAuthenticated(blueprint, "retired_operators"),
    fraudProofCatalogue: alwaysAuthenticated(
      blueprint,
      "fraud_proof_catalogue",
    ),
    computationThread: fieldPreimageCertificateV1,
    fraudProof: alwaysAuthenticated(blueprint, "fraud_proof"),
    chunkedVerify: reserve,
    pexcludes: reserve,
    eventHistory: null,
    deposit: alwaysAuthenticated(blueprint, "deposit"),
    withdrawal: alwaysAuthenticated(blueprint, "withdrawal"),
    txOrder: alwaysAuthenticated(blueprint, "tx_order"),
    fieldPreimageCertificate: fieldPreimageCertificateV1,
    scriptDecode,
    settlement: alwaysAuthenticated(blueprint, "settlement"),
    reserve,
    payout: alwaysAuthenticated(blueprint, "payout"),
    fraudProofContracts,
    fraudProofs,
  };
};
