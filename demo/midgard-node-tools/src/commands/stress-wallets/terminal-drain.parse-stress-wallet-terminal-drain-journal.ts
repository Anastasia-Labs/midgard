import { readFile } from "node:fs/promises";

import {
  computeMidgardNativeTxId,
  decodeMidgardNativeTxFullFromCanonicalCbor,
} from "@al-ft/midgard-core/codec/native";

import {
  asObject,
  assertExactKeys,
  requiredPositiveInteger,
  requiredString,
} from "./artifact-fields.js";
import { STRESS_WALLET_TERMINAL_DRAIN_JOURNAL_SCHEMA_VERSION } from "./constants.js";
import { fileExists } from "./files.js";
import { parseStressWalletNetwork } from "./options.js";
import { parseStressWalletOperationScope } from "./scope.js";
import {
  terminalDecimal,
  type TerminalDrainEntry,
  type TerminalDrainState,
  terminalExactString,
  terminalScopeHash,
  terminalSnapshotHash,
} from "./terminal-drain.terminal-snapshot-hash.js";

export const parseStressWalletTerminalDrainJournal = (
  value: unknown,
): TerminalDrainState => {
  const raw = asObject(value, "terminal drain state");
  assertExactKeys(raw, "terminal drain state", [
    "schemaVersion",
    "scope",
    "scopeSha256",
    "nodeEndpoint",
    "network",
    "treasuryAddress",
    "treasuryBeforeLovelace",
    "minFeeA",
    "minFeeB",
    "feeCapLovelace",
    "maxFeeIterations",
    "entries",
  ]);
  if (
    raw.schemaVersion !== STRESS_WALLET_TERMINAL_DRAIN_JOURNAL_SCHEMA_VERSION ||
    !Array.isArray(raw.entries)
  )
    throw new Error("Unsupported terminal drain journal schema.");
  const scope = parseStressWalletOperationScope(raw.scope);
  const walletIds = new Set<string>();
  const addresses = new Set<string>();
  const txHashes = new Set<string>();
  const entries = raw.entries.map((value, index): TerminalDrainEntry => {
    const e = asObject(value, "entries[" + index.toString() + "]");
    assertExactKeys(
      e,
      "entries[" + index.toString() + "]",
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
    if (
      !Array.isArray(e.beforeOutrefs) ||
      (e.selectedInputs !== undefined && !Array.isArray(e.selectedInputs))
    )
      throw new Error("Terminal drain input lists must be arrays.");
    const status = requiredString(e.status, "status");
    if (
      status !== "already_empty" &&
      status !== "prepared" &&
      status !== "committed"
    )
      throw new Error("Invalid terminal drain status.");
    const transactionFields = [
      e.txHash,
      e.signedTxCbor,
      e.selectedInputs,
      e.requestedLovelace,
      e.feeLovelace,
      e.signedTxBytes,
    ];
    if (
      status === "already_empty" &&
      transactionFields.some((field) => field !== undefined)
    )
      throw new Error(
        "Terminal drain already_empty entry contains forbidden transaction fields.",
      );
    if (
      status !== "already_empty" &&
      transactionFields.some((field) => field === undefined)
    )
      throw new Error("Terminal drain prepared/committed entry is incomplete.");
    const walletId = terminalExactString(e.walletId, "walletId");
    const address = terminalExactString(e.address, "address");
    if (walletIds.has(walletId) || addresses.has(address))
      throw new Error("Terminal drain wallet identities must be unique.");
    walletIds.add(walletId);
    addresses.add(address);
    const beforeOutrefs = e.beforeOutrefs.map((x) =>
      terminalExactString(x, "beforeOutref"),
    );
    if (
      beforeOutrefs.some(
        (outref) => !/^[0-9a-f]{64}#(0|[1-9]\d*)$/.test(outref),
      ) ||
      new Set(beforeOutrefs).size !== beforeOutrefs.length ||
      [...beforeOutrefs].sort().join("|") !== beforeOutrefs.join("|")
    )
      throw new Error(
        "Terminal drain beforeOutrefs must be unique sorted canonical outrefs.",
      );
    const beforeLovelace = terminalDecimal(e.beforeLovelace, "beforeLovelace");
    const beforeValueSha256 = terminalExactString(
      e.beforeValueSha256,
      "beforeValueSha256",
    );
    if (!/^[0-9a-f]{64}$/.test(beforeValueSha256))
      throw new Error(
        "Terminal drain beforeValueSha256 must be a lowercase SHA-256 digest.",
      );
    const parsed: TerminalDrainEntry = {
      walletId,
      address,
      beforeOutrefs,
      beforeLovelace,
      beforeValueSha256,
      status,
      ...(e.txHash === undefined
        ? {}
        : { txHash: terminalExactString(e.txHash, "txHash") }),
      ...(e.signedTxCbor === undefined
        ? {}
        : {
            signedTxCbor: terminalExactString(e.signedTxCbor, "signedTxCbor"),
          }),
      ...(e.selectedInputs === undefined
        ? {}
        : {
            selectedInputs: e.selectedInputs.map((x) =>
              terminalExactString(x, "selectedInput"),
            ),
          }),
      ...(e.requestedLovelace === undefined
        ? {}
        : {
            requestedLovelace: terminalDecimal(
              e.requestedLovelace,
              "requestedLovelace",
            ),
          }),
      ...(e.feeLovelace === undefined
        ? {}
        : { feeLovelace: terminalDecimal(e.feeLovelace, "feeLovelace") }),
      ...(e.signedTxBytes === undefined
        ? {}
        : {
            signedTxBytes: requiredPositiveInteger(
              e.signedTxBytes,
              "signedTxBytes",
            ),
          }),
    };
    if (status === "already_empty") {
      if (
        beforeOutrefs.length !== 0 ||
        beforeLovelace !== "0" ||
        beforeValueSha256 !== terminalSnapshotHash([])
      )
        throw new Error(
          "Terminal drain already_empty entry must bind the exact empty snapshot.",
        );
      return parsed;
    }
    const txHash = parsed.txHash!;
    const signedTxCbor = parsed.signedTxCbor!;
    const selectedInputs = parsed.selectedInputs!;
    if (
      !/^[0-9a-f]{64}$/.test(txHash) ||
      !/^[0-9a-f]+$/.test(signedTxCbor) ||
      signedTxCbor.length % 2 !== 0 ||
      txHashes.has(txHash)
    )
      throw new Error(
        "Terminal drain transaction identities must be unique lowercase canonical encodings.",
      );
    txHashes.add(txHash);
    if (
      selectedInputs.some(
        (outref) => !/^[0-9a-f]{64}#(0|[1-9]\d*)$/.test(outref),
      ) ||
      new Set(selectedInputs).size !== selectedInputs.length ||
      [...selectedInputs].sort().join("|") !== selectedInputs.join("|") ||
      selectedInputs.join("|") !== beforeOutrefs.join("|")
    )
      throw new Error(
        "Terminal drain selectedInputs must exactly equal the sorted snapshot outrefs.",
      );
    const signedTxBytes = Buffer.from(signedTxCbor, "hex");
    if (
      signedTxBytes.toString("hex") !== signedTxCbor ||
      signedTxBytes.length !== parsed.signedTxBytes
    )
      throw new Error(
        "Terminal drain signedTxBytes must bind exact signedTxCbor.",
      );
    let computedTxHash: string;
    try {
      computedTxHash = computeMidgardNativeTxId(
        decodeMidgardNativeTxFullFromCanonicalCbor(signedTxBytes),
      ).toString("hex");
    } catch (cause) {
      throw new Error(
        "Terminal drain signedTxCbor must be canonical Midgard native V1 transaction CBOR: " +
          String(cause),
      );
    }
    if (computedTxHash !== txHash)
      throw new Error("Terminal drain txHash must bind exact signedTxCbor.");
    if (
      BigInt(parsed.requestedLovelace!) + BigInt(parsed.feeLovelace!) !==
      BigInt(beforeLovelace)
    )
      throw new Error(
        "Terminal drain entry must conserve beforeLovelace as requestedLovelace+feeLovelace.",
      );
    return parsed;
  });
  const scopeSha256 = terminalExactString(raw.scopeSha256, "scopeSha256");
  if (
    !/^[0-9a-f]{64}$/.test(scopeSha256) ||
    scopeSha256 !== terminalScopeHash(scope)
  )
    throw new Error(
      "Terminal drain scopeSha256 must bind the exact parsed scope.",
    );
  return {
    schemaVersion: STRESS_WALLET_TERMINAL_DRAIN_JOURNAL_SCHEMA_VERSION,
    scope,
    scopeSha256,
    nodeEndpoint: terminalExactString(raw.nodeEndpoint, "nodeEndpoint"),
    network: parseStressWalletNetwork(
      requiredString(raw.network, "network"),
      {},
    ),
    treasuryAddress: terminalExactString(
      raw.treasuryAddress,
      "treasuryAddress",
    ),
    treasuryBeforeLovelace: terminalDecimal(
      raw.treasuryBeforeLovelace,
      "treasuryBeforeLovelace",
    ),
    minFeeA: terminalDecimal(raw.minFeeA, "minFeeA"),
    minFeeB: terminalDecimal(raw.minFeeB, "minFeeB"),
    feeCapLovelace: terminalDecimal(raw.feeCapLovelace, "feeCapLovelace"),
    maxFeeIterations: requiredPositiveInteger(
      raw.maxFeeIterations,
      "maxFeeIterations",
    ),
    entries,
  };
};

export const readTerminalDrainState = async (
  path: string,
): Promise<TerminalDrainState | undefined> => {
  if (!(await fileExists(path))) return undefined;
  return parseStressWalletTerminalDrainJournal(
    JSON.parse(await readFile(path, "utf8")) as unknown,
  );
};
