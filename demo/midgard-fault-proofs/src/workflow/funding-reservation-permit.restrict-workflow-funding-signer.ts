import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";
import {
  CML,
  type LucidEvolution,
  type UTxO,
  utxoToCore,
  type WalletApi,
} from "@lucid-evolution/lucid";

import type { ResolvedProverSigner } from "../runtime.js";
import {
  assertWorkflowActuationPermitIdentity,
  type WorkflowActuationPermit,
} from "./actuation-permit.js";
import { balanceCbor } from "./funding-reservation-permit.apply-transition.js";
import { assertFundingSubmissionAuthority } from "./funding-reservation-permit.begin-workflow-funding-reservation-action.js";
import {
  admittedPermits,
  WORKFLOW_FUNDING_RESERVATION_PERMIT,
  type WorkflowFundingReservationPermit,
  type WorkflowFundingReservationPort,
  type WorkflowFundingReservationSnapshot,
} from "./funding-reservation-permit.workflow-funding-reservation-port.js";

/**
 * Returns a signer whose wallet API exposes exactly the currently reserved
 * ordinary and collateral UTxOs. Signing and submission still delegate to the
 * original enterprise-key wallet.
 */
export const restrictWorkflowFundingSigner = ({
  signer,
  permit,
}: {
  readonly signer: ResolvedProverSigner;
  readonly permit: WorkflowFundingReservationPermit;
}): ResolvedProverSigner => {
  const state = admittedPermits.get(permit);
  if (state === undefined) {
    throw new Error("production funding reservation permit was not admitted");
  }
  if (
    signer.address !== state.snapshot.walletAddress ||
    signer.paymentKeyHash !== state.snapshot.fundingPaymentKeyHash
  ) {
    throw new Error("production signer differs from funding reservation");
  }
  const assertCurrentAction = (): void => {
    assertFundingSubmissionAuthority(state);
    if (state.currentActionKind === undefined)
      throw new Error("production signer used before a reserved action began");
  };
  return Object.freeze({
    ...signer,
    selectWallet: (lucid: LucidEvolution): void => {
      // Installing the API is safe before a resumed action has begun; every
      // funding read, transaction signature and submission checks its authority.
      signer.selectWallet(lucid);
      const original = lucid.wallet();
      // Builders may retain this Lucid wallet across prerequisite transactions.
      // Read the action refreshed by begin(), rather than the preceding action's
      // candidates, which may already have been spent by a publication.
      const inputs = (role: "funding" | "collateral"): readonly UTxO[] => {
        assertCurrentAction();
        const outRefs =
          role === "funding"
            ? state.currentFundingOutRefs
            : state.currentCollateralOutRefs;
        return outRefs.map((outRef) => state.resolvedInputs.get(outRef)!);
      };
      const addressHex = CML.Address.from_bech32(signer.address).to_hex();
      const api: WalletApi = Object.freeze({
        getNetworkId: async () =>
          CML.Address.from_bech32(signer.address).to_raw_bytes()[0]! & 0x0f,
        getUtxos: async () =>
          inputs("funding").map((utxo) => utxoToCore(utxo).to_cbor_hex()),
        getBalance: async () => balanceCbor(inputs("funding")),
        getUsedAddresses: async () => [addressHex],
        getUnusedAddresses: async () => [],
        getChangeAddress: async () => addressHex,
        getRewardAddresses: async () => [],
        signTx: async (tx) => {
          assertCurrentAction();
          return (
            await original.signTx(CML.Transaction.from_cbor_hex(tx))
          ).to_cbor_hex();
        },
        signData: async (address, payload) => {
          assertCurrentAction();
          return await original.signMessage(
            CML.Address.from_hex(address).to_bech32(),
            payload,
          );
        },
        submitTx: async (tx) => {
          assertCurrentAction();
          return await original.submitTx(tx);
        },
        getCollateral: async () =>
          inputs("collateral").map((utxo) => utxoToCore(utxo).to_cbor_hex()),
        experimental: Object.freeze({
          getCollateral: async () =>
            inputs("collateral").map((utxo) => utxoToCore(utxo).to_cbor_hex()),
          on: () => undefined,
          off: () => undefined,
        }),
      });
      lucid.selectWallet.fromAPI(api);
    },
  });
};

/** Test-only identity seam for runtime lifecycle tests that never build a tx. */
export const unsafeCreateWorkflowFundingReservationPermitForTest = ({
  category,
  actuationPermit,
  deploymentFingerprint,
  decisionDigest,
  rollbackGeneration,
}: {
  readonly category: FraudProofCatalogueCategoryName;
  readonly actuationPermit: WorkflowActuationPermit;
  readonly deploymentFingerprint: string;
  readonly decisionDigest: string;
  readonly rollbackGeneration: string;
}): WorkflowFundingReservationPermit => {
  if (process.env.NODE_ENV !== "test") {
    throw new Error("unsafe funding reservation permit is test-only");
  }
  const identity = assertWorkflowActuationPermitIdentity({
    permit: actuationPermit,
    category,
    rollbackGeneration,
  });
  if (
    identity.deploymentFingerprint !== deploymentFingerprint ||
    identity.decisionDigest !== decisionDigest
  ) {
    throw new Error("test funding reservation identity mismatch");
  }
  const permit: WorkflowFundingReservationPermit = Object.freeze({
    permitVersion: WORKFLOW_FUNDING_RESERVATION_PERMIT,
  });
  const snapshot: WorkflowFundingReservationSnapshot = Object.freeze({
    reservationId: "01".repeat(32),
    deploymentFingerprint,
    decisionDigest,
    policyDigest: "02".repeat(32),
    reservationBasisDigest: "03".repeat(32),
    rollbackGeneration,
    revision: "0",
    walletAddress: "test-only-no-wallet",
    fundingPaymentKeyHash: "04".repeat(28),
    state: "active",
    activeInputs: Object.freeze([]),
  });
  const port: WorkflowFundingReservationPort = Object.freeze({
    load: async () => snapshot,
    readPendingTransition: async () => null,
    readPendingHandoff: async () => null,
    readCompletionHandoff: async () => null,
    readAbandonmentHandoff: async () => null,
    acknowledgeAbandonment: async () => snapshot,
    resolveInputs: async () => [],
    resolveConfirmedInput: async () => {
      throw new Error("unsafe test permit has no confirmed action lineage");
    },
    resolveProtocolInputAuthority: async () => {
      throw new Error("unsafe test permit has no protocol input authority");
    },
    prepare: async () => snapshot,
    confirm: async () => snapshot,
    abandon: async () => snapshot,
    markConflict: async () => snapshot,
    release: async () => snapshot,
  });
  admittedPermits.set(permit, {
    category,
    policy: undefined,
    actuationPermit,
    port,
    maximumCollateralInputs: 0,
    idleReleaseAuthorized: false,
    snapshot,
    resolvedInputs: new Map(),
    boundJournal: undefined,
    currentActionKind: undefined,
    currentActionDigest: undefined,
    currentFundingOutRefs: Object.freeze([]),
    currentCollateralOutRefs: Object.freeze([]),
    pendingTransactionHash: undefined,
    preparedTransaction: undefined,
  });
  return permit;
};

export const unsafeWorkflowFundingReservationSelectedOutRefsForTest = (
  permit: WorkflowFundingReservationPermit,
): Readonly<{
  fundingOutRefs: readonly string[];
  collateralOutRefs: readonly string[];
}> => {
  if (process.env.NODE_ENV !== "test") {
    throw new Error("unsafe funding reservation inspection is test-only");
  }
  const state = admittedPermits.get(permit);
  if (state === undefined) {
    throw new Error("production funding reservation permit was not admitted");
  }
  return Object.freeze({
    fundingOutRefs: Object.freeze([...state.currentFundingOutRefs]),
    collateralOutRefs: Object.freeze([...state.currentCollateralOutRefs]),
  });
};
