import { createHash } from "node:crypto";

import { parseOutRefLabel } from "@al-ft/midgard-core/out-ref";
import {
  type AuthenticatedValidator,
  buildEventHistoryDeployments,
  buildHubOracleMintingValidator,
  buildTxOrderValidators,
  HUB_ORACLE_ASSET_NAME,
  parseFaultProofBlueprint,
} from "@al-ft/midgard-sdk";
import {
  CML,
  credentialToAddress,
  scriptHashToCredential,
} from "@lucid-evolution/lucid";

import { parseWatcherStrictJsonValue } from "./config.js";
import {
  fail,
  plainRecord,
} from "./deployment-identity.catalogue-category-to-contract.js";
import {
  authenticatedWatcherDeploymentIdentities,
  authenticatedWatcherProtocolScriptAuthorities,
  protocolScriptAuthorityByDeploymentIdentity,
  type SignedUserEventScript,
  USER_EVENT_SIGNED_CONTRACT_NAMES,
  userEventScriptsByDeploymentIdentity,
  type UserEventSignedContractName,
  type VerifiedWatcherDeploymentIdentity,
  type WatcherDeploymentProtocolScriptAuthority,
  watcherUserEventScriptBindingBrand,
} from "./deployment-identity.parse-trust-roots.js";

/** Live script-application authority only; this carries no activation or history proof. */
export type WatcherUserEventScriptBinding = Readonly<{
  [watcherUserEventScriptBindingBrand]: true;
}>;

type UserEventScriptIdentity = Readonly<{
  policyId: string;
  spendScriptHash: string;
  addressHex: string;
}>;

type WatcherUserEventScriptBindingDescription = Readonly<{
  deploymentFingerprint: string;
  blueprintHash: string;
  blueprintSha256: string;
  network: VerifiedWatcherDeploymentIdentity["network"];
  canonicalOneShotOutRef: string;
  hub: Readonly<{ policyId: string; assetName: string; addressHex: string }>;
  deposit: UserEventScriptIdentity;
  withdrawal: UserEventScriptIdentity;
  forcedOrder: UserEventScriptIdentity;
  certificatePolicyId: string;
}>;

const userEventScriptBindings = new WeakMap<
  object,
  Readonly<{
    deploymentIdentity: VerifiedWatcherDeploymentIdentity;
    description: WatcherUserEventScriptBindingDescription;
  }>
>();

const typedArrayByteLength = Object.getOwnPropertyDescriptor(
  Object.getPrototypeOf(Uint8Array.prototype),
  "byteLength",
)!.get!;

/** Admit the exact signed blueprint bytes and independently derive every event script. */
export const verifyWatcherUserEventScriptBinding = (input: {
  readonly deploymentIdentity: VerifiedWatcherDeploymentIdentity;
  readonly blueprintBytes: Uint8Array;
}): WatcherUserEventScriptBinding => {
  const { deploymentIdentity, blueprintBytes } = input;
  assertVerifiedWatcherDeploymentIdentity(deploymentIdentity);
  const signed =
    userEventScriptsByDeploymentIdentity.get(deploymentIdentity) ??
    fail("invalid_field", "$.verifiedDeploymentIdentity.userEventScripts");
  const byteLength = (() => {
    try {
      return typedArrayByteLength.call(blueprintBytes) as number;
    } catch {
      return fail("invalid_field", "$.blueprintBytes");
    }
  })();
  if (
    !(blueprintBytes instanceof Uint8Array) ||
    byteLength < 1 ||
    byteLength > 64 * 1024 * 1024
  ) {
    fail("invalid_field", "$.blueprintBytes");
  }
  const bytes = new Uint8Array(blueprintBytes);
  if (
    createHash("sha256").update(bytes).digest("hex") !== signed.blueprintSha256
  ) {
    fail("mismatched_identity", "$.blueprintBytes.sha256");
  }
  const blueprint = (() => {
    try {
      const raw = plainRecord(
        parseWatcherStrictJsonValue(
          new TextDecoder("utf-8", { fatal: true }).decode(bytes),
        ),
        "$.blueprint",
      );
      if (
        !Array.isArray(raw.validators) ||
        raw.validators.length < 1 ||
        raw.validators.length > 4096
      ) {
        fail("invalid_field", "$.blueprint.validators");
      }
      return parseFaultProofBlueprint(raw);
    } catch {
      return fail("invalid_field", "$.blueprint");
    }
  })();
  const protocol = watcherDeploymentProtocolScriptAuthority(deploymentIdentity);
  const derived = (() => {
    try {
      const hub = buildHubOracleMintingValidator({
        blueprint,
        oneShotOutRef: parseOutRefLabel(protocol.hubOracleOneShotOutRef),
      });
      const parameters = {
        blueprint,
        network: deploymentIdentity.network,
        hubOraclePolicyId: hub.policyId,
      };
      const historyFor = (name: "deposit" | "withdrawal") => {
        const recipe = signed.historyRecipes[name];
        if (
          recipe.hubPolicyId !== hub.policyId ||
          recipe.kind !== (name === "deposit" ? "Deposit" : "Withdrawal") ||
          `${recipe.initializationNonce.txHash}#${recipe.initializationNonce.outputIndex}` !==
            protocol.hubOracleOneShotOutRef
        )
          throw new Error(
            "History recipe differs from its canonical deployment",
          );
        return buildEventHistoryDeployments({
          ...parameters,
          initializationNonce: recipe.initializationNonce,
          protectionDurationMs: BigInt(recipe.protectionDurationMs),
          bounds: {
            inlineLimitBytes: BigInt(recipe.bounds.inlineLimitBytes),
            maxPayloadBytes: BigInt(recipe.bounds.maxPayloadBytes),
            maxPayloadNodes: BigInt(recipe.bounds.maxPayloadNodes),
          },
        })[name];
      };
      const depositHistory = historyFor("deposit");
      const withdrawalHistory = historyFor("withdrawal");
      return {
        hub,
        depositHistory,
        withdrawalHistory,
        deposit: depositHistory.list,
        withdrawal: withdrawalHistory.list,
        ...buildTxOrderValidators(parameters),
      };
    } catch {
      return fail("invalid_field", "$.blueprint.userEventScripts");
    }
  })();
  const actual: Record<UserEventSignedContractName, SignedUserEventScript> = {
    hubOracleMint: {
      type: derived.hub.mintingScript.type,
      cborHex: derived.hub.mintingScriptCBOR,
      scriptHash: derived.hub.policyId,
    },
    depositMint: {
      type: derived.deposit.mintingScript.type,
      cborHex: derived.deposit.mintingScriptCBOR,
      scriptHash: derived.deposit.policyId,
    },
    depositSpend: {
      type: derived.deposit.spendingScript.type,
      cborHex: derived.deposit.spendingScriptCBOR,
      scriptHash: derived.deposit.spendingScriptHash,
    },
    withdrawalMint: {
      type: derived.withdrawal.mintingScript.type,
      cborHex: derived.withdrawal.mintingScriptCBOR,
      scriptHash: derived.withdrawal.policyId,
    },
    withdrawalSpend: {
      type: derived.withdrawal.spendingScript.type,
      cborHex: derived.withdrawal.spendingScriptCBOR,
      scriptHash: derived.withdrawal.spendingScriptHash,
    },
    depositHistoryRetentionSpend: {
      type: derived.depositHistory.retention.spendingScript.type,
      cborHex: derived.depositHistory.retention.spendingScriptCBOR,
      scriptHash: derived.depositHistory.retention.spendingScriptHash,
    },
    depositHistoryRetirementWithdraw: {
      type: derived.depositHistory.retirement.withdrawalScript.type,
      cborHex: derived.depositHistory.retirement.withdrawalScriptCBOR,
      scriptHash: derived.depositHistory.retirement.withdrawalScriptHash,
    },
    withdrawalHistoryRetentionSpend: {
      type: derived.withdrawalHistory.retention.spendingScript.type,
      cborHex: derived.withdrawalHistory.retention.spendingScriptCBOR,
      scriptHash: derived.withdrawalHistory.retention.spendingScriptHash,
    },
    withdrawalHistoryRetirementWithdraw: {
      type: derived.withdrawalHistory.retirement.withdrawalScript.type,
      cborHex: derived.withdrawalHistory.retirement.withdrawalScriptCBOR,
      scriptHash: derived.withdrawalHistory.retirement.withdrawalScriptHash,
    },
    txOrderMint: {
      type: derived.txOrder.mintingScript.type,
      cborHex: derived.txOrder.mintingScriptCBOR,
      scriptHash: derived.txOrder.policyId,
    },
    txOrderSpend: {
      type: derived.txOrder.spendingScript.type,
      cborHex: derived.txOrder.spendingScriptCBOR,
      scriptHash: derived.txOrder.spendingScriptHash,
    },
    fieldPreimageCertificateMint: {
      type: derived.fieldPreimageCertificate.mintingScript.type,
      cborHex: derived.fieldPreimageCertificate.mintingScriptCBOR,
      scriptHash: derived.fieldPreimageCertificate.policyId,
    },
  };
  for (const name of USER_EVENT_SIGNED_CONTRACT_NAMES) {
    const expected = signed.contracts[name];
    const script = actual[name];
    if (
      expected.type !== "PlutusV3" ||
      script.type !== expected.type ||
      script.cborHex !== expected.cborHex ||
      script.scriptHash !== expected.scriptHash
    ) {
      fail("mismatched_identity", `$.manifest.contracts.${name}`);
    }
  }
  const eventIdentity = (
    validator: AuthenticatedValidator,
  ): UserEventScriptIdentity =>
    Object.freeze({
      policyId: validator.policyId,
      spendScriptHash: validator.spendingScriptHash,
      addressHex: CML.Address.from_bech32(
        validator.spendingScriptAddress,
      ).to_hex(),
    });
  const description: WatcherUserEventScriptBindingDescription = Object.freeze({
    deploymentFingerprint: deploymentIdentity.manifestId,
    blueprintHash: deploymentIdentity.blueprintHash,
    blueprintSha256: signed.blueprintSha256,
    network: deploymentIdentity.network,
    canonicalOneShotOutRef: protocol.hubOracleOneShotOutRef,
    hub: Object.freeze({
      policyId: derived.hub.policyId,
      assetName: HUB_ORACLE_ASSET_NAME,
      addressHex: CML.Address.from_bech32(
        credentialToAddress(
          deploymentIdentity.network,
          scriptHashToCredential(derived.hub.policyId),
        ),
      ).to_hex(),
    }),
    deposit: eventIdentity(derived.deposit),
    withdrawal: eventIdentity(derived.withdrawal),
    forcedOrder: eventIdentity(derived.txOrder),
    certificatePolicyId: derived.fieldPreimageCertificate.policyId,
  });
  const binding = Object.freeze({}) as WatcherUserEventScriptBinding;
  userEventScriptBindings.set(
    binding,
    Object.freeze({ deploymentIdentity: deploymentIdentity, description }),
  );
  return binding;
};

/** The returned frozen description cannot be substituted for the admitted handle. */
export const readWatcherUserEventScriptBinding = (input: {
  readonly binding: WatcherUserEventScriptBinding;
  readonly deploymentIdentity: VerifiedWatcherDeploymentIdentity;
}): WatcherUserEventScriptBindingDescription => {
  const { binding, deploymentIdentity } = input;
  assertVerifiedWatcherDeploymentIdentity(deploymentIdentity);
  const admitted = userEventScriptBindings.get(binding);
  if (
    admitted === undefined ||
    admitted.deploymentIdentity !== deploymentIdentity
  ) {
    return fail("invalid_field", "$.userEventScriptBinding");
  }
  return admitted.description;
};

/**
 * Refuses structural casts at production authority boundaries. Only the
 * signature/policy verifier in this module can admit an identity object.
 */
export const assertVerifiedWatcherDeploymentIdentity = (
  identity: VerifiedWatcherDeploymentIdentity,
): void => {
  if (!authenticatedWatcherDeploymentIdentities.has(identity)) {
    fail("invalid_field", "$.verifiedDeploymentIdentity");
  }
};

/**
 * Refuses a structural policy/hash bundle. Only the signed deployment verifier
 * can mint the script authority consumed by production state-queue sources.
 */
export const assertWatcherDeploymentProtocolScriptAuthority = (
  authority: WatcherDeploymentProtocolScriptAuthority,
): void => {
  if (!authenticatedWatcherProtocolScriptAuthorities.has(authority)) {
    fail("invalid_field", "$.protocolScriptAuthority");
  }
};

export const watcherDeploymentProtocolScriptAuthority = (
  identity: VerifiedWatcherDeploymentIdentity,
): WatcherDeploymentProtocolScriptAuthority => {
  assertVerifiedWatcherDeploymentIdentity(identity);
  return (
    protocolScriptAuthorityByDeploymentIdentity.get(identity) ??
    fail("invalid_field", "$.verifiedDeploymentIdentity.protocolScripts")
  );
};
