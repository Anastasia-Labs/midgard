import { parseDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";

import {
  assertIsoString,
  assertLowerHex,
  assertString,
  assertStringArray,
  assertStringRecord,
  type DeploymentRunEvent,
  type DeploymentRunIdentity,
  type DeploymentStepState,
  exactRunStateRecord,
  parseStepStatus,
  RunStateError,
} from "./run-state.deployment-run-identity.js";

export const parseDeploymentRunIdentity = (
  value: unknown,
): DeploymentRunIdentity => {
  const input = exactRunStateRecord(
    value,
    "identity",
    [],
    [
      "network",
      "hubOracleOneShot",
      "referenceScriptAuthPolicyId",
      "referenceScriptAuthPolicy",
      "manifestPath",
      "manifestSha256",
      "deploymentMarker",
    ],
  );
  const hubOracleOneShot =
    input.hubOracleOneShot === undefined
      ? undefined
      : exactRunStateRecord(
          input.hubOracleOneShot,
          "identity.hubOracleOneShot",
          ["txHash", "outputIndex"],
        );
  const referenceScriptAuthPolicy =
    input.referenceScriptAuthPolicy === undefined
      ? undefined
      : exactRunStateRecord(
          input.referenceScriptAuthPolicy,
          "identity.referenceScriptAuthPolicy",
          ["policyId", "nativeScript"],
        );
  const referenceScriptAuthPolicyNativeScript =
    referenceScriptAuthPolicy === undefined
      ? undefined
      : exactRunStateRecord(
          referenceScriptAuthPolicy.nativeScript,
          "identity.referenceScriptAuthPolicy.nativeScript",
          [
            "type",
            "cborHex",
            "expiresAtSlot",
            "expiresAtUnixTime",
            "timelockDurationMs",
          ],
        );
  const parsed: DeploymentRunIdentity = {
    ...(input.network === undefined
      ? {}
      : { network: assertString(input.network, "identity.network") }),
    ...(hubOracleOneShot === undefined
      ? {}
      : {
          hubOracleOneShot: {
            txHash: assertLowerHex(
              hubOracleOneShot.txHash,
              "identity.hubOracleOneShot.txHash",
              32,
            ),
            outputIndex:
              typeof hubOracleOneShot.outputIndex === "number" &&
              Number.isSafeInteger(hubOracleOneShot.outputIndex) &&
              hubOracleOneShot.outputIndex >= 0
                ? hubOracleOneShot.outputIndex
                : (() => {
                    throw new RunStateError(
                      "identity.hubOracleOneShot.outputIndex must be a non-negative integer.",
                    );
                  })(),
          },
        }),
    ...(input.referenceScriptAuthPolicyId === undefined
      ? {}
      : {
          referenceScriptAuthPolicyId: assertLowerHex(
            input.referenceScriptAuthPolicyId,
            "identity.referenceScriptAuthPolicyId",
            28,
          ),
        }),
    ...(referenceScriptAuthPolicy === undefined
      ? {}
      : {
          referenceScriptAuthPolicy: {
            policyId: assertLowerHex(
              referenceScriptAuthPolicy.policyId,
              "identity.referenceScriptAuthPolicy.policyId",
              28,
            ),
            nativeScript: {
              type:
                referenceScriptAuthPolicyNativeScript?.type === "Native"
                  ? "Native"
                  : (() => {
                      throw new RunStateError(
                        "identity.referenceScriptAuthPolicy.nativeScript.type must be Native.",
                      );
                    })(),
              cborHex: (() => {
                const cborHex = assertString(
                  referenceScriptAuthPolicyNativeScript.cborHex,
                  "identity.referenceScriptAuthPolicy.nativeScript.cborHex",
                );
                if (cborHex.length % 2 !== 0 || !/^[0-9a-f]+$/u.test(cborHex)) {
                  throw new RunStateError(
                    "identity.referenceScriptAuthPolicy.nativeScript.cborHex must be non-empty even-length lowercase hexadecimal.",
                  );
                }
                return cborHex;
              })(),
              expiresAtSlot:
                typeof referenceScriptAuthPolicyNativeScript.expiresAtSlot ===
                  "number" &&
                Number.isSafeInteger(
                  referenceScriptAuthPolicyNativeScript.expiresAtSlot,
                ) &&
                referenceScriptAuthPolicyNativeScript.expiresAtSlot >= 0
                  ? referenceScriptAuthPolicyNativeScript.expiresAtSlot
                  : (() => {
                      throw new RunStateError(
                        "identity.referenceScriptAuthPolicy.nativeScript.expiresAtSlot must be a non-negative safe integer.",
                      );
                    })(),
              expiresAtUnixTime:
                typeof referenceScriptAuthPolicyNativeScript.expiresAtUnixTime ===
                  "number" &&
                Number.isSafeInteger(
                  referenceScriptAuthPolicyNativeScript.expiresAtUnixTime,
                ) &&
                referenceScriptAuthPolicyNativeScript.expiresAtUnixTime > 0
                  ? referenceScriptAuthPolicyNativeScript.expiresAtUnixTime
                  : (() => {
                      throw new RunStateError(
                        "identity.referenceScriptAuthPolicy.nativeScript.expiresAtUnixTime must be a positive safe integer.",
                      );
                    })(),
              timelockDurationMs:
                typeof referenceScriptAuthPolicyNativeScript.timelockDurationMs ===
                  "number" &&
                Number.isSafeInteger(
                  referenceScriptAuthPolicyNativeScript.timelockDurationMs,
                ) &&
                referenceScriptAuthPolicyNativeScript.timelockDurationMs > 0
                  ? referenceScriptAuthPolicyNativeScript.timelockDurationMs
                  : (() => {
                      throw new RunStateError(
                        "identity.referenceScriptAuthPolicy.nativeScript.timelockDurationMs must be a positive safe integer.",
                      );
                    })(),
            },
          },
        }),
    ...(input.manifestPath === undefined
      ? {}
      : {
          manifestPath: assertString(
            input.manifestPath,
            "identity.manifestPath",
          ),
        }),
    ...(input.manifestSha256 === undefined
      ? {}
      : {
          manifestSha256: assertLowerHex(
            input.manifestSha256,
            "identity.manifestSha256",
            32,
          ),
        }),
    ...(input.deploymentMarker === undefined
      ? {}
      : {
          deploymentMarker: (() => {
            try {
              return parseDeploymentMarker(input.deploymentMarker);
            } catch (cause) {
              throw new RunStateError(
                `identity.deploymentMarker is invalid: ${
                  cause instanceof Error ? cause.message : String(cause)
                }`,
                { cause },
              );
            }
          })(),
        }),
  };
  if (
    parsed.referenceScriptAuthPolicyId !== undefined &&
    parsed.referenceScriptAuthPolicy !== undefined &&
    parsed.referenceScriptAuthPolicyId !==
      parsed.referenceScriptAuthPolicy.policyId
  ) {
    throw new RunStateError(
      "identity reference-script policy identifiers are inconsistent.",
    );
  }
  return parsed;
};

export const parseDeploymentStepState = (
  value: unknown,
  label = "step",
): DeploymentStepState => {
  const input = exactRunStateRecord(
    value,
    label,
    ["status", "updatedAt"],
    ["txHashes", "outRefs", "message", "evidence", "details"],
  );
  const parsed: DeploymentStepState = {
    status: parseStepStatus(input.status),
    updatedAt: assertIsoString(input.updatedAt, `${label}.updatedAt`),
    ...(input.txHashes === undefined
      ? {}
      : {
          txHashes: assertStringArray(input.txHashes, `${label}.txHashes`),
        }),
    ...(input.outRefs === undefined
      ? {}
      : { outRefs: assertStringArray(input.outRefs, `${label}.outRefs`) }),
    ...(input.message === undefined
      ? {}
      : { message: assertString(input.message, `${label}.message`) }),
    ...(input.evidence === undefined
      ? {}
      : {
          evidence: assertStringArray(input.evidence, `${label}.evidence`),
        }),
    ...(input.details === undefined
      ? {}
      : { details: assertStringRecord(input.details, `${label}.details`) }),
  };
  for (const [index, txHash] of (parsed.txHashes ?? []).entries()) {
    assertLowerHex(txHash, `${label}.txHashes[${index.toString()}]`, 32);
  }
  for (const [index, outRef] of (parsed.outRefs ?? []).entries()) {
    if (!/^[0-9a-f]{64}#(0|[1-9]\d*)$/u.test(outRef)) {
      throw new RunStateError(
        `${label}.outRefs[${index.toString()}] must be a canonical transaction output reference.`,
      );
    }
  }
  return parsed;
};

export const parseDeploymentRunEvent = (
  value: unknown,
  label = "event",
): DeploymentRunEvent => {
  const input = exactRunStateRecord(
    value,
    label,
    ["at", "kind", "message"],
    ["stepId"],
  );
  return {
    at: assertIsoString(input.at, `${label}.at`),
    kind:
      input.kind === "created" || input.kind === "step_transition"
        ? input.kind
        : (() => {
            throw new RunStateError(
              `${label}.kind must be created or step_transition.`,
            );
          })(),
    message: assertString(input.message, `${label}.message`),
    ...(input.stepId === undefined
      ? {}
      : {
          stepId: assertString(input.stepId, `${label}.stepId`),
        }),
  };
};

export const parseEvents = (value: unknown): readonly DeploymentRunEvent[] => {
  if (!Array.isArray(value)) {
    throw new RunStateError("events must be an array.");
  }
  return value.map((entry, index) =>
    parseDeploymentRunEvent(entry, `events[${index.toString()}]`),
  );
};
