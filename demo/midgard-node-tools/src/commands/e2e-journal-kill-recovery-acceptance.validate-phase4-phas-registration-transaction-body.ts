import { createHash } from "node:crypto";
import { existsSync, readFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { CML } from "@lucid-evolution/lucid";

import {
  type Phase4PhasRegistrationProof,
  type Phase4ProcessIsolationIdentity,
} from "./e2e-journal-kill-recovery-acceptance.validate-phase4-process-isolation-values.js";

export const validatePhase4PhasRegistrationTransactionBody = (
  value: unknown,
  proof: Phase4PhasRegistrationProof,
): Phase4ProcessIsolationIdentity["snapshotPhasRegistrationTransactionBody"] => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error("Phase 4 PHAS transaction-body envelope must be an object");
  }
  const envelope = value as {
    readonly type?: unknown;
    readonly description?: unknown;
    readonly cborHex?: unknown;
  };
  if (
    Object.keys(envelope)
      .sort((left, right) => left.localeCompare(right))
      .join(",") !== "cborHex,description,type"
  ) {
    throw new Error(
      "Phase 4 PHAS transaction-body envelope fields do not match the exact schema",
    );
  }
  if (
    envelope.type !== "Unwitnessed Tx ConwayEra" ||
    typeof envelope.description !== "string" ||
    envelope.description.length === 0 ||
    typeof envelope.cborHex !== "string" ||
    envelope.cborHex.length === 0 ||
    envelope.cborHex.length % 2 !== 0 ||
    !/^[a-f0-9]+$/u.test(envelope.cborHex)
  ) {
    throw new Error(
      "Phase 4 PHAS transaction-body envelope is not exact canonical unsigned CBOR",
    );
  }
  const cborBytes = Buffer.from(envelope.cborHex, "hex");
  const transaction = CML.Transaction.from_cbor_hex(envelope.cborHex);
  const body = transaction.body();
  const certificates = body.certs();
  const certificate = certificates?.len() === 1 ? certificates.get(0) : null;
  const credential = certificate?.as_stake_registration()?.stake_credential();
  if (
    createHash("sha256").update(cborBytes).digest("hex") !==
      proof.transactionBody.cborSha256 ||
    cborBytes.length !== proof.transactionBody.cborSizeBytes ||
    transaction.to_canonical_cbor_hex() !== envelope.cborHex ||
    transaction.witness_set().to_cbor_hex() !== "a0" ||
    CML.hash_transaction(body).to_hex() !== proof.registrationTxHash ||
    certificate?.kind() !== CML.CertificateKind.StakeRegistration ||
    credential?.kind() !== CML.CredentialKind.Script ||
    credential.as_script()?.to_hex() !== proof.scriptHash
  ) {
    throw new Error(
      "Phase 4 PHAS unsigned transaction body does not contain the exact submitted script registration certificate",
    );
  }
  return envelope as Phase4ProcessIsolationIdentity["snapshotPhasRegistrationTransactionBody"];
};

/**
 * This package's own root, located from the running module so the same code
 * resolves it from the built bundle (`dist/index.js`) and from source under
 * vitest. The devnet assets and the summary verifier live here, not in the
 * node root the acceptance run points at.
 */
export const toolsPackageRoot = (): string => {
  let directory = dirname(fileURLToPath(import.meta.url));
  for (;;) {
    const candidate = join(directory, "package.json");
    if (existsSync(candidate)) {
      const manifest = JSON.parse(readFileSync(candidate, "utf8")) as {
        readonly name?: unknown;
      };
      if (manifest.name === "midgard-node-tools") {
        return directory;
      }
    }
    const parent = dirname(directory);
    if (parent === directory) {
      throw new Error(
        "midgard-node-tools package root not found above the acceptance module",
      );
    }
    directory = parent;
  }
};

/** The built tooling CLI, used to re-enter this package for gated steps. */
export const toolsCli = (): string =>
  resolve(toolsPackageRoot(), "dist/index.js");
