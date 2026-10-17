import {
  artifactDecimal,
  artifactExactString,
  asObject,
  assertExactKeys,
  parseExactVersionedArtifact,
} from "./artifact-fields.js";
import { parseStressWalletAccountingArtifact } from "./artifacts.parse-stress-wallet-fanout-artifact.js";
import { STRESS_WALLET_TERMINAL_DRAIN_REPORT_SCHEMA_VERSION } from "./constants.js";
import { parseStressWalletNetwork } from "./options.js";
import { parseStressWalletOperationScope } from "./scope.js";

export const parseStressWalletTerminalDrainReport = (
  value: unknown,
): Record<string, unknown> => {
  const label = "stress wallet terminal drain report";
  const raw = parseExactVersionedArtifact(
    value,
    label,
    STRESS_WALLET_TERMINAL_DRAIN_REPORT_SCHEMA_VERSION,
    [
      "scope",
      "statePath",
      "endpoint",
      "network",
      "treasuryAddress",
      "conservation",
      "wallets",
    ],
  );
  const scope = parseStressWalletOperationScope(raw.scope);
  ["statePath", "endpoint", "treasuryAddress"].forEach((name) =>
    artifactExactString(raw[name], `${label} ${name}`),
  );
  const networkText = artifactExactString(raw.network, `${label} network`);
  parseStressWalletNetwork(networkText, {});
  const conservation = asObject(raw.conservation, `${label} conservation`);
  assertExactKeys(conservation, `${label} conservation`, [
    "grossSourceLovelace",
    "treasuryDeltaLovelace",
    "totalFeesLovelace",
    "residualSourceLovelace",
  ]);
  const gross = artifactDecimal(
    conservation.grossSourceLovelace,
    `${label} conservation.grossSourceLovelace`,
  );
  const delta = artifactDecimal(
    conservation.treasuryDeltaLovelace,
    `${label} conservation.treasuryDeltaLovelace`,
  );
  const fees = artifactDecimal(
    conservation.totalFeesLovelace,
    `${label} conservation.totalFeesLovelace`,
  );
  const residual = artifactDecimal(
    conservation.residualSourceLovelace,
    `${label} conservation.residualSourceLovelace`,
  );
  if (residual !== "0" || BigInt(delta) + BigInt(fees) !== BigInt(gross)) {
    throw new Error(`${label} conservation binding is inconsistent.`);
  }
  if (!Array.isArray(raw.wallets) || raw.wallets.length !== scope.count) {
    throw new Error(`${label} wallet cardinality is inconsistent.`);
  }
  const walletIds = new Set<string>();
  for (const [index, value] of raw.wallets.entries()) {
    const walletLabel = `${label} wallets[${index.toString()}]`;
    const wallet = asObject(value, walletLabel);
    assertExactKeys(wallet, walletLabel, [
      "walletId",
      "address",
      "before",
      "after",
    ]);
    const walletId = artifactExactString(
      wallet.walletId,
      `${walletLabel}.walletId`,
    );
    if (walletIds.has(walletId)) {
      throw new Error(`${label} wallet IDs must be unique.`);
    }
    walletIds.add(walletId);
    artifactExactString(wallet.address, `${walletLabel}.address`);
    const before = asObject(wallet.before, `${walletLabel}.before`);
    assertExactKeys(
      before,
      `${walletLabel}.before`,
      [
        "walletId",
        "address",
        "beforeOutrefs",
        "beforeLovelace",
        "beforeValueSha256",
        "status",
      ],
      [
        "txHash",
        "signedTxCbor",
        "selectedInputs",
        "requestedLovelace",
        "feeLovelace",
        "signedTxBytes",
      ],
    );
    if (before.walletId !== walletId || before.address !== wallet.address) {
      throw new Error(`${walletLabel}.before identity is mismatched.`);
    }
    parseStressWalletAccountingArtifact(wallet.after, `${walletLabel}.after`);
  }
  return raw;
};
