import { createHash } from "node:crypto";
import {
  chmod,
  mkdir,
  open,
  readdir,
  readFile,
  rename,
} from "node:fs/promises";
import { dirname, isAbsolute, join } from "node:path";

import { compileUPLC, parseUPLC } from "@harmoniclabs/uplc";
import { CML } from "@lucid-evolution/lucid";

type ByteStringLayer = {
  readonly headerHex: string;
  readonly payloadHex: string;
  readonly payloadLength: number;
  readonly totalLength: number;
  readonly rawPrefix: string;
};

type CmlScript = {
  readonly to_cbor_hex?: () => string;
  readonly to_raw_bytes?: () => Uint8Array;
  readonly hash?: () => { to_hex: () => string };
};

type InspectedScript = {
  readonly location: string;
  readonly cmlType: string;
  readonly languageTag?: number;
  readonly cborHexPrefix?: string;
  readonly cborBytes?: number;
  readonly nestedCborLayers?: ReturnType<typeof inspectByteStringLayers>;
  readonly rawBytes?: number;
  readonly rawPrefix?: string;
  readonly rawHex: string;
  readonly canonicalDecodeValid: boolean;
  readonly canonicalDecodeError?: string;
  readonly flatDecodeValid?: boolean;
  readonly flatDecodeError?: string;
  readonly computedHash?: string;
};

const hexByte = (hex: string, offset: number): number =>
  Number.parseInt(hex.slice(offset, offset + 2), 16);

const readByteStringLayer = (inputHex: string): ByteStringLayer | undefined => {
  const hex = inputHex.toLowerCase();
  if (hex.length < 2 || !/^[0-9a-f]+$/.test(hex)) return undefined;
  const first = hexByte(hex, 0);
  if (first >> 5 !== 2) return undefined;
  const additional = first & 0x1f;
  let headerLength = 1;
  let payloadLength: number;
  if (additional < 24) {
    payloadLength = additional;
  } else if (additional >= 24 && additional <= 27) {
    const lengthBytes = 2 ** (additional - 24);
    if (hex.length < (1 + lengthBytes) * 2) return undefined;
    headerLength += lengthBytes;
    const lengthHex = hex.slice(2, (1 + lengthBytes) * 2);
    payloadLength = Number.parseInt(lengthHex, 16);
    if (!Number.isSafeInteger(payloadLength)) return undefined;
  } else {
    return undefined;
  }
  const payloadStart = headerLength * 2;
  const payloadEnd = payloadStart + payloadLength * 2;
  if (hex.length < payloadEnd) return undefined;
  return {
    headerHex: hex.slice(0, payloadStart),
    payloadHex: hex.slice(payloadStart, payloadEnd),
    payloadLength,
    totalLength: headerLength + payloadLength,
    rawPrefix: hex.slice(payloadStart, Math.min(payloadEnd, payloadStart + 16)),
  };
};

const inspectByteStringLayers = (cborHex: string) => {
  const layers: ByteStringLayer[] = [];
  let remainder = cborHex.toLowerCase();
  let trailingHex = "";
  while (true) {
    const layer = readByteStringLayer(remainder);
    if (layer === undefined) break;
    layers.push(layer);
    const suffix = remainder.slice(layer.totalLength * 2);
    if (suffix.length !== 0) {
      trailingHex = suffix;
      break;
    }
    remainder = layer.payloadHex;
    if (remainder.length === 0) break;
  }
  return {
    layerCount: layers.length,
    layers: layers.map((layer) => ({
      headerHex: layer.headerHex,
      payloadLength: layer.payloadLength,
      rawPrefix: layer.rawPrefix,
    })),
    trailingHex,
  };
};

const bytesToHex = (bytes: Uint8Array): string =>
  Buffer.from(bytes).toString("hex");

const expectedUplcVersion = (languageTag: number): string => {
  switch (languageTag) {
    case 1:
    case 2:
      return "1.0.0";
    case 3:
      return "1.1.0";
    default:
      throw new Error(`Unsupported Plutus language tag ${languageTag}`);
  }
};

export const validateLedgerSerializedFlat = (
  ledgerSerializedHex: string,
  languageTag: number,
): void => {
  const layer = readByteStringLayer(ledgerSerializedHex);
  if (layer === undefined) {
    throw new Error(
      "Ledger serialized script must contain exactly one definite CBOR byte-string layer",
    );
  }
  if (layer.totalLength * 2 !== ledgerSerializedHex.length) {
    throw new Error("Ledger serialized script has trailing bytes");
  }
  if (layer.payloadLength === 0) {
    throw new Error("Ledger serialized script contains an empty Flat program");
  }
  const extraLayer = readByteStringLayer(layer.payloadHex);
  if (
    extraLayer !== undefined &&
    extraLayer.totalLength * 2 === layer.payloadHex.length
  ) {
    throw new Error("Ledger serialized script contains an extra CBOR layer");
  }

  const flatBytes = Buffer.from(layer.payloadHex, "hex");
  const program = parseUPLC(flatBytes, "flat");
  const actualVersion = program.version.toString();
  const requiredVersion = expectedUplcVersion(languageTag);
  if (actualVersion !== requiredVersion) {
    throw new Error(
      `PlutusV${languageTag} script uses unsupported UPLC version ${actualVersion}; expected ${requiredVersion}`,
    );
  }

  const reencoded = compileUPLC(program, {
    trivialOptimization: false,
  }).toBuffer();
  if (
    reencoded.nZeroesAsEndPadding !== 0 ||
    bytesToHex(reencoded.buffer) !== layer.payloadHex
  ) {
    throw new Error(
      "Flat decoder did not consume the complete canonical program encoding",
    );
  }
};

const inspectPlutusScript = (
  script: CmlScript,
  cmlType: string,
  languageTag: number,
  location: string,
): InspectedScript => {
  const cborHex = script.to_cbor_hex?.() ?? "";
  const layers = inspectByteStringLayers(cborHex);
  const rawBytes = script.to_raw_bytes?.();
  const rawHex = rawBytes === undefined ? undefined : bytesToHex(rawBytes);
  let canonicalDecodeValid = false;
  let canonicalDecodeError: string | undefined;
  let flatDecodeValid = false;
  let flatDecodeError: string | undefined;
  try {
    parseUPLC(Buffer.from(cborHex, "hex"), "cbor");
    canonicalDecodeValid = true;
  } catch (error) {
    canonicalDecodeError = String(error);
  }
  if (rawHex !== undefined) {
    try {
      validateLedgerSerializedFlat(rawHex, languageTag);
      flatDecodeValid = true;
    } catch (error) {
      flatDecodeError = String(error);
    }
  }
  return {
    location,
    cmlType,
    languageTag,
    cborHexPrefix: cborHex.slice(0, 24),
    cborBytes: cborHex.length / 2,
    nestedCborLayers: layers,
    rawBytes: rawHex === undefined ? undefined : rawHex.length / 2,
    rawPrefix: rawHex?.slice(0, 24),
    rawHex: rawHex ?? "",
    canonicalDecodeValid,
    canonicalDecodeError,
    flatDecodeValid,
    flatDecodeError,
    computedHash: script.hash?.().to_hex(),
  };
};

const inspectNativeScript = (
  script: CmlScript,
  location: string,
): InspectedScript => {
  const cborHex = script.to_cbor_hex?.() ?? "";
  const rawHex =
    script.to_raw_bytes === undefined
      ? cborHex
      : bytesToHex(script.to_raw_bytes());
  return {
    location,
    cmlType: "NativeScript",
    cborHexPrefix: cborHex.slice(0, 24),
    cborBytes: cborHex.length / 2,
    rawBytes: rawHex.length / 2,
    rawPrefix: rawHex.slice(0, 24),
    rawHex,
    canonicalDecodeValid: cborHex.length > 0,
    computedHash: script.hash?.().to_hex(),
  };
};

const collectionEntries = (collection: unknown): unknown[] => {
  if (collection === null || collection === undefined) return [];
  const value = collection as {
    len?: () => number;
    get?: (i: number) => unknown;
  };
  if (typeof value.len !== "function" || typeof value.get !== "function")
    return [];
  return Array.from({ length: value.len() }, (_, index) => value.get!(index));
};

const inspectTransaction = (signedTxCbor: string) => {
  const tx = CML.Transaction.from_cbor_hex(signedTxCbor);
  const body = tx.body();
  const scriptRefs = [];
  const outputs = body.outputs();
  for (let index = 0; index < outputs.len(); index += 1) {
    const scriptRef = outputs.get(index).script_ref();
    if (scriptRef === null || scriptRef === undefined) continue;
    const native = scriptRef.as_native();
    if (native !== undefined && native !== null) {
      scriptRefs.push(
        inspectNativeScript(native, `body.outputs[${index}].script_ref`),
      );
      continue;
    }
    const v1 = scriptRef.as_plutus_v1();
    const v2 =
      v1 === undefined || v1 === null ? scriptRef.as_plutus_v2() : undefined;
    const v3 =
      v1 === undefined || v1 === null
        ? v2 === undefined || v2 === null
          ? scriptRef.as_plutus_v3()
          : undefined
        : undefined;
    const languageTag =
      v1 !== undefined && v1 !== null
        ? 1
        : v2 !== undefined && v2 !== null
          ? 2
          : 3;
    const plutus = v1 ?? v2 ?? v3;
    if (plutus === undefined || plutus === null) continue;
    scriptRefs.push(
      inspectPlutusScript(
        plutus,
        "ScriptRef",
        languageTag,
        `body.outputs[${index}].script_ref`,
      ),
    );
  }
  const witnesses = tx.witness_set();
  const witnessScripts = [
    ...collectionEntries(witnesses.plutus_v1_scripts()).map((script, index) =>
      inspectPlutusScript(
        script as CmlScript,
        "PlutusV1Script",
        1,
        `witness.plutus_v1_scripts[${index}]`,
      ),
    ),
    ...collectionEntries(witnesses.plutus_v2_scripts()).map((script, index) =>
      inspectPlutusScript(
        script as CmlScript,
        "PlutusV2Script",
        2,
        `witness.plutus_v2_scripts[${index}]`,
      ),
    ),
    ...collectionEntries(witnesses.plutus_v3_scripts()).map((script, index) =>
      inspectPlutusScript(
        script as CmlScript,
        "PlutusV3Script",
        3,
        `witness.plutus_v3_scripts[${index}]`,
      ),
    ),
    ...collectionEntries(witnesses.native_scripts()).map((script, index) =>
      inspectNativeScript(
        script as CmlScript,
        `witness.native_scripts[${index}]`,
      ),
    ),
  ];
  return {
    cmlTransactionType: "Transaction",
    bodyHash: CML.hash_transaction(body).to_hex(),
    bodyScriptRefs: scriptRefs,
    witnessScripts,
  };
};

export type SignedTxPreSubmitCapture = {
  readonly outputDirectory: string;
  readonly invocation: "phase4-live-pre-submit-capture";
  readonly abortBeforeSubmit: true;
  readonly session: {
    readonly commandName: string;
    readonly runStatePath: string;
    readonly blueprintPath: string;
    readonly blueprintSha256: string;
    readonly ledgerProtocolMajor: number;
    readonly network: string;
    readonly hubOracleOneShotOutRef: string;
    readonly referenceScriptAuthPolicyId: string;
  };
};

export type SignedTxPreSubmitBatchContext = {
  readonly ordinal: number;
  readonly plannedBatchIndex: number;
  readonly splitPath: string;
  readonly targets: readonly {
    readonly name: string;
    readonly scriptHash: string;
    readonly outputIndex: number;
  }[];
  readonly inputs: readonly {
    readonly outRef: string;
    readonly lineage: "live_seed" | "synthetic_change";
  }[];
  readonly walletChangeOutputIndexes: readonly number[];
};

export type CapturedSignedTxNotSubmitted = {
  readonly status: "captured_not_submitted";
  readonly txHash: string;
  readonly signedTxCbor: string;
  readonly walletAddress: string;
  readonly cborPath: string;
  readonly metadataPath: string;
};

export type SignedTxPreSubmitCaptureComplete = {
  readonly schemaVersion: 1;
  readonly status: "complete";
  readonly completePath: string;
  readonly expectedTargetCount: number;
  readonly captureCount: number;
  readonly targetNames: readonly string[];
};

const assertCaptureConfig = (capture: SignedTxPreSubmitCapture): void => {
  if (!isAbsolute(capture.outputDirectory)) {
    throw new Error("Pre-submit capture output directory must be absolute");
  }
  if (
    capture.invocation !== "phase4-live-pre-submit-capture" ||
    capture.abortBeforeSubmit !== true
  ) {
    throw new Error(
      "Pre-submit capture requires the explicit aborting diagnostic invocation",
    );
  }
  if (
    !isAbsolute(capture.session.runStatePath) ||
    !isAbsolute(capture.session.blueprintPath) ||
    !/^[0-9a-f]{64}$/i.test(capture.session.blueprintSha256) ||
    !Number.isSafeInteger(capture.session.ledgerProtocolMajor) ||
    capture.session.ledgerProtocolMajor <= 0 ||
    !/^[0-9a-f]{56}$/i.test(capture.session.referenceScriptAuthPolicyId) ||
    !/^[0-9a-f]{64}#\d+$/i.test(capture.session.hubOracleOneShotOutRef) ||
    capture.session.commandName.length === 0 ||
    capture.session.network.length === 0
  ) {
    throw new Error("Pre-submit capture session identity is incomplete");
  }
};

export const assertSignedTxPreSubmitCaptureCliSafety = ({
  capture,
  freshRedeploy,
  planOnly,
}: {
  readonly capture: SignedTxPreSubmitCapture;
  readonly freshRedeploy: boolean;
  readonly planOnly: boolean;
}): void => {
  assertCaptureConfig(capture);
  if (freshRedeploy) {
    throw new Error(
      "--capture-signed-tx-pre-submit cannot be combined with --fresh-redeploy because diagnostic mode must bind an already-persisted auth policy identity",
    );
  }
  if (planOnly) {
    throw new Error(
      "--capture-signed-tx-pre-submit cannot be combined with --plan-only",
    );
  }
};

export const prepareSignedTxPreSubmitCaptureDirectory = async (
  capture: SignedTxPreSubmitCapture,
): Promise<void> => {
  assertCaptureConfig(capture);
  await mkdir(capture.outputDirectory, { recursive: false, mode: 0o700 });
  await chmod(capture.outputDirectory, 0o700);
  const parent = await open(dirname(capture.outputDirectory), "r");
  try {
    await parent.sync();
  } finally {
    await parent.close();
  }
};

export const assertPersistedSignedTxPreSubmitCaptureIdentity = async (
  capture: SignedTxPreSubmitCapture,
): Promise<void> => {
  assertCaptureConfig(capture);
  const parsed = JSON.parse(
    await readFile(capture.session.runStatePath, "utf8"),
  ) as {
    readonly schemaVersion?: unknown;
    readonly identity?: {
      readonly network?: unknown;
      readonly hubOracleOneShot?: {
        readonly txHash?: unknown;
        readonly outputIndex?: unknown;
      };
      readonly referenceScriptAuthPolicyId?: unknown;
      readonly referenceScriptAuthPolicy?: {
        readonly policyId?: unknown;
      };
    };
  };
  const identity = parsed.identity;
  const persistedOutRef =
    typeof identity?.hubOracleOneShot?.txHash === "string" &&
    Number.isSafeInteger(identity.hubOracleOneShot.outputIndex)
      ? `${identity.hubOracleOneShot.txHash}#${String(identity.hubOracleOneShot.outputIndex)}`
      : undefined;
  if (
    parsed.schemaVersion !== "midgard-deployment-run-state-v1" ||
    identity?.network !== capture.session.network ||
    persistedOutRef !== capture.session.hubOracleOneShotOutRef ||
    identity?.referenceScriptAuthPolicyId !==
      capture.session.referenceScriptAuthPolicyId ||
    identity?.referenceScriptAuthPolicy?.policyId !==
      capture.session.referenceScriptAuthPolicyId
  ) {
    throw new Error(
      "Pre-submit capture requires persisted run-state network, hub one-shot, and reference-script auth identity to exactly match the resolved capture session",
    );
  }
};

const assertBatchContext = (batch: SignedTxPreSubmitBatchContext): void => {
  if (
    !Number.isSafeInteger(batch.ordinal) ||
    batch.ordinal < 0 ||
    !Number.isSafeInteger(batch.plannedBatchIndex) ||
    batch.plannedBatchIndex < 0 ||
    batch.splitPath.length === 0 ||
    batch.targets.length === 0
  ) {
    throw new Error("Pre-submit capture batch context is incomplete");
  }
  const targetNames = batch.targets.map(({ name }) => name);
  const outputIndexes = batch.targets.map(({ outputIndex }) => outputIndex);
  if (
    new Set(targetNames).size !== targetNames.length ||
    new Set(outputIndexes).size !== outputIndexes.length ||
    batch.targets.some(
      ({ name, scriptHash, outputIndex }) =>
        name.length === 0 ||
        !/^[0-9a-f]{56}$/i.test(scriptHash) ||
        !Number.isSafeInteger(outputIndex) ||
        outputIndex < 0,
    )
  ) {
    throw new Error(
      "Pre-submit capture batch has missing or duplicate target identity",
    );
  }
  if (
    batch.inputs.length === 0 ||
    new Set(batch.inputs.map(({ outRef }) => outRef)).size !==
      batch.inputs.length ||
    batch.inputs.some(
      ({ outRef, lineage }) =>
        !/^[0-9a-f]{64}#\d+$/i.test(outRef) ||
        (lineage !== "live_seed" && lineage !== "synthetic_change"),
    ) ||
    new Set(batch.walletChangeOutputIndexes).size !==
      batch.walletChangeOutputIndexes.length ||
    batch.walletChangeOutputIndexes.some(
      (outputIndex) =>
        !Number.isSafeInteger(outputIndex) ||
        outputIndex < 0 ||
        outputIndexes.includes(outputIndex),
    )
  ) {
    throw new Error(
      "Pre-submit capture batch input lineage is missing or duplicated",
    );
  }
};

const inspectedScriptOutputIndex = (script: InspectedScript): number => {
  const match = /^body\.outputs\[(\d+)]\.script_ref$/.exec(script.location);
  if (match === null) {
    throw new Error(
      `Captured body reference script has an invalid location: ${script.location}`,
    );
  }
  return Number(match[1]);
};

const assertInspectedScriptsValid = (
  tx: ReturnType<typeof inspectTransaction>,
  batch: SignedTxPreSubmitBatchContext,
): void => {
  const inspectedByOutput = new Map(
    tx.bodyScriptRefs.map((script) => [
      inspectedScriptOutputIndex(script),
      script,
    ]),
  );
  if (inspectedByOutput.size !== tx.bodyScriptRefs.length) {
    throw new Error(
      "Captured transaction contains duplicate script-ref outputs",
    );
  }
  const expectedOutputs = new Set(
    batch.targets.map(({ outputIndex }) => outputIndex),
  );
  if (
    inspectedByOutput.size !== expectedOutputs.size ||
    [...inspectedByOutput.keys()].some((index) => !expectedOutputs.has(index))
  ) {
    throw new Error(
      "Captured transaction body reference scripts do not exactly match the declared batch targets",
    );
  }
  const allScripts = [...tx.bodyScriptRefs, ...tx.witnessScripts];
  for (const script of allScripts) {
    if (
      !script.canonicalDecodeValid ||
      script.rawHex.length === 0 ||
      script.computedHash === undefined ||
      (script.cmlType !== "NativeScript" && script.flatDecodeValid !== true)
    ) {
      throw new Error(
        `Captured script validation failed at ${script.location}: canonical=${String(script.canonicalDecodeValid)},flat=${String(script.flatDecodeValid)},hash=${script.computedHash ?? "missing"},canonical_error=${script.canonicalDecodeError ?? "none"},flat_error=${script.flatDecodeError ?? "none"}`,
      );
    }
  }
  for (const target of batch.targets) {
    const script = inspectedByOutput.get(target.outputIndex);
    if (script?.computedHash !== target.scriptHash) {
      throw new Error(
        `Captured script hash mismatch for ${target.name}: expected=${target.scriptHash},actual=${script?.computedHash ?? "missing"}`,
      );
    }
  }
};

export const captureSignedTxPreSubmit = async ({
  signedTxCbor,
  txHash,
  walletAddress,
  label,
  capture,
  batch,
}: {
  readonly signedTxCbor: string;
  readonly txHash: string;
  readonly walletAddress: string;
  readonly label?: string;
  readonly capture: SignedTxPreSubmitCapture;
  readonly batch: SignedTxPreSubmitBatchContext;
}): Promise<CapturedSignedTxNotSubmitted> => {
  assertCaptureConfig(capture);
  assertBatchContext(batch);
  if ((await readdir(capture.outputDirectory)).includes("COMPLETE.json")) {
    throw new Error("Pre-submit capture directory is already complete");
  }
  if (!/^[0-9a-f]+$/i.test(signedTxCbor) || signedTxCbor.length % 2 !== 0) {
    throw new Error(
      "Signed transaction CBOR must be an even-length hexadecimal string",
    );
  }
  const bytes = Buffer.from(signedTxCbor, "hex");
  const sha256 = createHash("sha256").update(bytes).digest("hex");
  const tx = inspectTransaction(signedTxCbor);
  if (tx.bodyHash !== txHash) {
    throw new Error(
      `Signed transaction body hash ${tx.bodyHash} does not match precomputed txHash ${txHash}`,
    );
  }
  assertInspectedScriptsValid(tx, batch);
  const captureBasename = `signed-${txHash}.cbor`;
  const cborPath = join(capture.outputDirectory, captureBasename);
  const metadataPath = join(capture.outputDirectory, `${captureBasename}.json`);
  const payloads: Array<{
    readonly targetName: string;
    readonly outputIndex: number;
    readonly languageTag?: number;
    readonly cmlType: string;
    readonly computedHash: string;
    readonly payloadPath: string;
    readonly payloadSha256: string;
    readonly payloadBytes: number;
    readonly payload: Buffer;
  }> = [];
  for (const target of batch.targets) {
    const script = tx.bodyScriptRefs.find(
      (candidate) =>
        inspectedScriptOutputIndex(candidate) === target.outputIndex,
    )!;
    const payloadBytes = Buffer.from(script.rawHex, "hex");
    const payloadSha256 = createHash("sha256")
      .update(payloadBytes)
      .digest("hex");
    const payloadPath = join(
      capture.outputDirectory,
      `payload-${txHash}-${target.outputIndex.toString()}.cbor`,
    );
    payloads.push({
      targetName: target.name,
      outputIndex: target.outputIndex,
      languageTag: script.languageTag,
      cmlType: script.cmlType,
      computedHash: script.computedHash!,
      payloadPath,
      payloadSha256,
      payloadBytes: payloadBytes.length,
      payload: payloadBytes,
    });
  }
  const metadata = {
    schemaVersion: 2,
    status: "captured_not_submitted",
    capturedAt: new Date().toISOString(),
    invocation: capture.invocation,
    abortBeforeSubmit: true,
    txHash,
    signedTxSha256: sha256,
    cborBytes: bytes.length,
    walletAddress,
    label,
    session: capture.session,
    batch,
    outputs: {
      referenceScripts: batch.targets.map(
        ({ name, scriptHash, outputIndex }) => ({
          targetName: name,
          scriptHash,
          outputIndex,
          outRef: `${txHash}#${outputIndex.toString()}`,
        }),
      ),
      walletChange: batch.walletChangeOutputIndexes.map((outputIndex) => ({
        outputIndex,
        outRef: `${txHash}#${outputIndex.toString()}`,
      })),
    },
    payloads: payloads.map(({ payload: _, ...entry }) => entry),
    cborPath,
    ...{
      ...tx,
      bodyScriptRefs: tx.bodyScriptRefs.map(
        ({ rawHex: _, ...script }) => script,
      ),
      witnessScripts: tx.witnessScripts.map(
        ({ rawHex: _, ...script }) => script,
      ),
    },
  };
  const writePrivateFile = async (path: string, content: string | Buffer) => {
    const handle = await open(path, "wx", 0o600);
    try {
      await handle.writeFile(content);
      await handle.sync();
    } finally {
      await handle.close();
    }
    await chmod(path, 0o600);
  };
  for (let index = 0; index < payloads.length; index += 1) {
    await writePrivateFile(
      payloads[index]!.payloadPath,
      payloads[index]!.payload,
    );
  }
  await writePrivateFile(cborPath, bytes);
  // Keep the CBOR if metadata persistence fails: it is the primary forensic artifact.
  await writePrivateFile(
    metadataPath,
    `${JSON.stringify(metadata, null, 2)}\n`,
  );
  const directoryHandle = await open(capture.outputDirectory, "r");
  try {
    await directoryHandle.sync();
  } finally {
    await directoryHandle.close();
  }
  return {
    status: "captured_not_submitted",
    txHash,
    signedTxCbor,
    walletAddress,
    cborPath,
    metadataPath,
  };
};

type PersistedCaptureMetadata = {
  readonly schemaVersion: 2;
  readonly status: "captured_not_submitted";
  readonly invocation: "phase4-live-pre-submit-capture";
  readonly abortBeforeSubmit: true;
  readonly txHash: string;
  readonly bodyHash: string;
  readonly signedTxSha256: string;
  readonly cborPath: string;
  readonly session: SignedTxPreSubmitCapture["session"];
  readonly batch: SignedTxPreSubmitBatchContext;
  readonly outputs: {
    readonly referenceScripts: readonly {
      readonly targetName: string;
      readonly scriptHash: string;
      readonly outputIndex: number;
      readonly outRef: string;
    }[];
    readonly walletChange: readonly {
      readonly outputIndex: number;
      readonly outRef: string;
    }[];
  };
  readonly payloads: readonly {
    readonly targetName: string;
    readonly outputIndex: number;
    readonly languageTag?: number;
    readonly cmlType: string;
    readonly computedHash: string;
    readonly payloadPath: string;
    readonly payloadSha256: string;
    readonly payloadBytes: number;
  }[];
};

const sameSession = (
  left: SignedTxPreSubmitCapture["session"],
  right: SignedTxPreSubmitCapture["session"],
): boolean => JSON.stringify(left) === JSON.stringify(right);

const readCaptureMetadata = async (
  path: string,
): Promise<PersistedCaptureMetadata> => {
  const parsed = JSON.parse(await readFile(path, "utf8")) as unknown;
  if (
    typeof parsed !== "object" ||
    parsed === null ||
    (parsed as { schemaVersion?: unknown }).schemaVersion !== 2 ||
    (parsed as { status?: unknown }).status !== "captured_not_submitted" ||
    (parsed as { abortBeforeSubmit?: unknown }).abortBeforeSubmit !== true ||
    (parsed as { invocation?: unknown }).invocation !==
      "phase4-live-pre-submit-capture"
  ) {
    throw new Error(`Invalid pre-submit capture metadata: ${path}`);
  }
  return parsed as PersistedCaptureMetadata;
};

export const finalizeSignedTxPreSubmitCapture = async ({
  capture,
  expectedTargetNames,
}: {
  readonly capture: SignedTxPreSubmitCapture;
  readonly expectedTargetNames: readonly string[];
}): Promise<SignedTxPreSubmitCaptureComplete> => {
  assertCaptureConfig(capture);
  if (
    expectedTargetNames.length === 0 ||
    new Set(expectedTargetNames).size !== expectedTargetNames.length
  ) {
    throw new Error(
      "Pre-submit capture completion requires unique expected targets",
    );
  }
  const entries = await readdir(capture.outputDirectory);
  if (entries.includes("COMPLETE.json")) {
    throw new Error("Pre-submit capture directory is already complete");
  }
  const metadataPaths = entries
    .filter((entry) => /^signed-[0-9a-f]{64}\.cbor\.json$/i.test(entry))
    .sort()
    .map((entry) => join(capture.outputDirectory, entry));
  if (metadataPaths.length === 0) {
    throw new Error("Pre-submit capture has no captured transactions");
  }
  const metadata = await Promise.all(metadataPaths.map(readCaptureMetadata));
  const seenTxHashes = new Set<string>();
  const seenOrdinals = new Set<number>();
  const targetCounts = new Map<string, number>();
  const completeCaptures = [];
  for (let index = 0; index < metadata.length; index += 1) {
    const entry = metadata[index]!;
    assertBatchContext(entry.batch);
    if (!sameSession(entry.session, capture.session)) {
      throw new Error(
        `Pre-submit capture session mismatch in ${metadataPaths[index]}`,
      );
    }
    if (
      !/^[0-9a-f]{64}$/i.test(entry.txHash) ||
      entry.bodyHash !== entry.txHash ||
      seenTxHashes.has(entry.txHash) ||
      seenOrdinals.has(entry.batch.ordinal)
    ) {
      throw new Error(
        `Pre-submit capture has invalid or duplicate transaction identity in ${metadataPaths[index]}`,
      );
    }
    seenTxHashes.add(entry.txHash);
    seenOrdinals.add(entry.batch.ordinal);
    const signedBytes = await readFile(entry.cborPath);
    const expectedCborPath = join(
      capture.outputDirectory,
      `signed-${entry.txHash}.cbor`,
    );
    if (
      entry.cborPath !== expectedCborPath ||
      createHash("sha256").update(signedBytes).digest("hex") !==
        entry.signedTxSha256
    ) {
      throw new Error(
        `Pre-submit capture signed CBOR hash/path mismatch for ${entry.txHash}`,
      );
    }
    const reinspected = inspectTransaction(signedBytes.toString("hex"));
    if (reinspected.bodyHash !== entry.txHash) {
      throw new Error(
        `Pre-submit capture body hash changed before completion for ${entry.txHash}`,
      );
    }
    assertInspectedScriptsValid(reinspected, entry.batch);
    if (
      entry.payloads.length !== entry.batch.targets.length ||
      entry.outputs.referenceScripts.length !== entry.batch.targets.length
    ) {
      throw new Error(
        `Pre-submit capture payload/output coverage mismatch for ${entry.txHash}`,
      );
    }
    for (const target of entry.batch.targets) {
      targetCounts.set(target.name, (targetCounts.get(target.name) ?? 0) + 1);
      const payload = entry.payloads.find(
        (candidate) =>
          candidate.targetName === target.name &&
          candidate.outputIndex === target.outputIndex,
      );
      const output = entry.outputs.referenceScripts.find(
        (candidate) =>
          candidate.targetName === target.name &&
          candidate.outputIndex === target.outputIndex,
      );
      if (
        payload === undefined ||
        output === undefined ||
        payload.computedHash !== target.scriptHash ||
        output.scriptHash !== target.scriptHash ||
        output.outRef !== `${entry.txHash}#${target.outputIndex.toString()}` ||
        payload.payloadPath !==
          join(
            capture.outputDirectory,
            `payload-${entry.txHash}-${target.outputIndex.toString()}.cbor`,
          )
      ) {
        throw new Error(
          `Pre-submit capture target identity mismatch for ${target.name}`,
        );
      }
      const payloadBytes = await readFile(payload.payloadPath);
      const reinspectedScript = reinspected.bodyScriptRefs.find(
        (candidate) =>
          inspectedScriptOutputIndex(candidate) === target.outputIndex,
      )!;
      if (
        payloadBytes.length !== payload.payloadBytes ||
        createHash("sha256").update(payloadBytes).digest("hex") !==
          payload.payloadSha256 ||
        payloadBytes.toString("hex") !== reinspectedScript.rawHex
      ) {
        throw new Error(
          `Pre-submit capture payload hash mismatch for ${target.name}`,
        );
      }
    }
    completeCaptures.push({
      ordinal: entry.batch.ordinal,
      plannedBatchIndex: entry.batch.plannedBatchIndex,
      splitPath: entry.batch.splitPath,
      txHash: entry.txHash,
      signedTxSha256: entry.signedTxSha256,
      cborPath: entry.cborPath,
      inputs: entry.batch.inputs,
      outputs: entry.outputs,
      targets: entry.batch.targets.map((target) => {
        const payload = entry.payloads.find(
          (candidate) => candidate.targetName === target.name,
        )!;
        return {
          ...target,
          languageTag: payload.languageTag,
          cmlType: payload.cmlType,
          payloadPath: payload.payloadPath,
          payloadSha256: payload.payloadSha256,
          payloadBytes: payload.payloadBytes,
        };
      }),
    });
  }
  const ordinals = [...seenOrdinals].sort((left, right) => left - right);
  if (ordinals.some((ordinal, index) => ordinal !== index)) {
    throw new Error("Pre-submit capture ordinals are not contiguous from zero");
  }
  const expected = new Set(expectedTargetNames);
  const missingOrDuplicate = expectedTargetNames.filter(
    (name) => targetCounts.get(name) !== 1,
  );
  const unexpected = [...targetCounts.keys()].filter(
    (name) => !expected.has(name),
  );
  if (missingOrDuplicate.length > 0 || unexpected.length > 0) {
    throw new Error(
      `Pre-submit capture target coverage is incomplete: missing_or_duplicate=[${missingOrDuplicate.join(",")}],unexpected=[${unexpected.join(",")}]`,
    );
  }
  const completePath = join(capture.outputDirectory, "COMPLETE.json");
  const temporaryPath = join(
    capture.outputDirectory,
    `.COMPLETE.${process.pid.toString()}.${Date.now().toString()}.tmp`,
  );
  const complete = {
    schemaVersion: 1 as const,
    status: "complete" as const,
    completedAt: new Date().toISOString(),
    invocation: capture.invocation,
    abortBeforeSubmit: true,
    session: capture.session,
    expectedTargetCount: expectedTargetNames.length,
    captureCount: completeCaptures.length,
    targetNames: [...expectedTargetNames],
    captures: completeCaptures.sort(
      (left, right) => left.ordinal - right.ordinal,
    ),
  };
  const handle = await open(temporaryPath, "wx", 0o600);
  try {
    await handle.writeFile(`${JSON.stringify(complete, null, 2)}\n`);
    await handle.sync();
  } finally {
    await handle.close();
  }
  await rename(temporaryPath, completePath);
  await chmod(completePath, 0o600);
  const directoryHandle = await open(capture.outputDirectory, "r");
  try {
    await directoryHandle.sync();
  } finally {
    await directoryHandle.close();
  }
  return {
    schemaVersion: 1,
    status: "complete",
    completePath,
    expectedTargetCount: expectedTargetNames.length,
    captureCount: completeCaptures.length,
    targetNames: [...expectedTargetNames],
  };
};
