import { createHash } from "node:crypto";
import { constants } from "node:fs";
import {
  chmod,
  link,
  lstat,
  mkdir,
  open,
  readdir,
  readFile,
  realpath,
  unlink,
} from "node:fs/promises";
import { basename, dirname, isAbsolute, join, resolve } from "node:path";

import { compileUPLC, parseUPLC } from "@harmoniclabs/uplc";
import { CML, getAddressDetails } from "@lucid-evolution/lucid";

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

type CmlVkeyWitness = {
  readonly vkey: () => {
    readonly hash: () => { readonly to_hex: () => string };
    readonly verify: (message: Uint8Array, signature: unknown) => boolean;
  };
  readonly ed25519_signature: () => unknown;
};

type InspectedVkeyWitness = {
  readonly location: string;
  readonly keyHash?: string;
  readonly signatureValid: boolean;
  readonly signatureError?: string;
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
  if (
    (additional === 24 && payloadLength < 24) ||
    (additional === 25 && payloadLength < 0x100) ||
    (additional === 26 && payloadLength < 0x1_0000) ||
    (additional === 27 && payloadLength < 0x1_0000_0000)
  ) {
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

const CAPTURE_SESSION_BASENAME = ".CAPTURE_SESSION.json";

let privateFileTemporaryOrdinal = 0;

const writePrivateFileAtomically = async (
  path: string,
  content: string | Buffer,
): Promise<void> => {
  privateFileTemporaryOrdinal += 1;
  const temporaryPath = join(
    dirname(path),
    `.${basename(path)}.${process.pid.toString()}.${Date.now().toString()}.${privateFileTemporaryOrdinal.toString()}.tmp`,
  );
  try {
    const handle = await open(temporaryPath, "wx", 0o600);
    try {
      await handle.writeFile(content);
      await handle.chmod(0o600);
      await handle.sync();
    } finally {
      await handle.close();
    }
    // link(2) installs the completed inode atomically and refuses to replace an
    // existing destination. rename(2) would silently overwrite it.
    await link(temporaryPath, path);
  } catch (error) {
    try {
      await unlink(temporaryPath);
    } catch {
      // A crash may still leave a temporary inode. Finalization rejects every
      // unrecognized entry, so cleanup failure can never produce COMPLETE.
    }
    throw error;
  }
  try {
    await unlink(temporaryPath);
  } catch (error) {
    throw new Error(
      `Installed pre-submit capture artifact but failed to unlink temporary path ${temporaryPath}: ${String(error)}`,
    );
  }
};

const syncDirectory = async (path: string): Promise<void> => {
  const handle = await open(path, "r");
  try {
    await handle.sync();
  } finally {
    await handle.close();
  }
};

const readPrivateRegularFile = async (path: string): Promise<Buffer> => {
  const handle = await open(path, constants.O_RDONLY | constants.O_NOFOLLOW);
  try {
    const fileStat = await handle.stat();
    if (
      !fileStat.isFile() ||
      fileStat.nlink !== 1 ||
      (fileStat.mode & 0o777) !== 0o600
    ) {
      throw new Error(
        `Pre-submit capture artifact must be a single-link private regular file with mode 0600: ${path}`,
      );
    }
    return await handle.readFile();
  } finally {
    await handle.close();
  }
};

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

const inspectVkeyWitness = (
  witness: CmlVkeyWitness,
  index: number,
  bodyHash: Uint8Array,
): InspectedVkeyWitness => {
  try {
    const vkey = witness.vkey();
    return {
      location: `witness.vkeywitnesses[${index.toString()}]`,
      keyHash: vkey.hash().to_hex(),
      signatureValid: vkey.verify(bodyHash, witness.ed25519_signature()),
    };
  } catch (error) {
    return {
      location: `witness.vkeywitnesses[${index.toString()}]`,
      signatureValid: false,
      signatureError: String(error),
    };
  }
};

const inspectTransaction = (signedTxCbor: string) => {
  const tx = CML.Transaction.from_cbor_hex(signedTxCbor);
  const body = tx.body();
  const bodyHash = CML.hash_transaction(body).to_hex();
  const bodyHashBytes = Buffer.from(bodyHash, "hex");
  const bodyInputs = body.inputs();
  const bodyInputOutRefs = Array.from(
    { length: bodyInputs.len() },
    (_, index) => {
      const input = bodyInputs.get(index);
      return `${input.transaction_id().to_hex()}#${input.index().toString()}`;
    },
  );
  const scriptRefs = [];
  const outputs = body.outputs();
  const bodyOutputAddresses = Array.from(
    { length: outputs.len() },
    (_, index) => bytesToHex(outputs.get(index).address().to_raw_bytes()),
  );
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
  const vkeyWitnesses = collectionEntries(witnesses.vkeywitnesses()).map(
    (witness, index) =>
      inspectVkeyWitness(witness as CmlVkeyWitness, index, bodyHashBytes),
  );
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
    canonicalCborRoundTrip: tx.to_cbor_hex() === signedTxCbor.toLowerCase(),
    bodyHash,
    bodyInputOutRefs,
    bodyOutputAddresses,
    bodyScriptRefs: scriptRefs,
    vkeyWitnesses,
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
  if (
    !isAbsolute(capture.outputDirectory) ||
    resolve(capture.outputDirectory) !== capture.outputDirectory
  ) {
    throw new Error(
      "Pre-submit capture output directory must be absolute and lexically canonical",
    );
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
    resolve(capture.session.runStatePath) !== capture.session.runStatePath ||
    !isAbsolute(capture.session.blueprintPath) ||
    resolve(capture.session.blueprintPath) !== capture.session.blueprintPath ||
    !/^[0-9a-f]{64}$/.test(capture.session.blueprintSha256) ||
    !Number.isSafeInteger(capture.session.ledgerProtocolMajor) ||
    capture.session.ledgerProtocolMajor <= 0 ||
    !/^[0-9a-f]{56}$/.test(capture.session.referenceScriptAuthPolicyId) ||
    !/^[0-9a-f]{64}#\d+$/.test(capture.session.hubOracleOneShotOutRef) ||
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

const assertSignedTxPreSubmitCaptureSourceIdentity = async (
  capture: SignedTxPreSubmitCapture,
): Promise<void> => {
  await assertPersistedSignedTxPreSubmitCaptureIdentity(capture);
  const blueprintBytes = await readFile(capture.session.blueprintPath);
  const blueprintSha256 = createHash("sha256")
    .update(blueprintBytes)
    .digest("hex");
  if (blueprintSha256 !== capture.session.blueprintSha256) {
    throw new Error(
      `Pre-submit capture blueprint hash mismatch: expected=${capture.session.blueprintSha256},actual=${blueprintSha256}`,
    );
  }
};

const captureSessionPath = (capture: SignedTxPreSubmitCapture): string =>
  join(capture.outputDirectory, CAPTURE_SESSION_BASENAME);

const sameSession = (
  left: SignedTxPreSubmitCapture["session"],
  right: SignedTxPreSubmitCapture["session"],
): boolean => JSON.stringify(left) === JSON.stringify(right);

const assertPreparedCaptureDirectory = async (
  capture: SignedTxPreSubmitCapture,
): Promise<void> => {
  const resolvedDirectory = await realpath(capture.outputDirectory);
  const directoryStat = await lstat(capture.outputDirectory);
  if (
    resolvedDirectory !== capture.outputDirectory ||
    !directoryStat.isDirectory() ||
    (directoryStat.mode & 0o777) !== 0o700
  ) {
    throw new Error(
      "Pre-submit capture directory must remain a canonical private directory with mode 0700",
    );
  }
  const sessionPath = captureSessionPath(capture);
  const parsed = JSON.parse(
    (await readPrivateRegularFile(sessionPath)).toString("utf8"),
  ) as {
    readonly schemaVersion?: unknown;
    readonly status?: unknown;
    readonly invocation?: unknown;
    readonly abortBeforeSubmit?: unknown;
    readonly outputDirectory?: unknown;
    readonly session?: SignedTxPreSubmitCapture["session"];
  };
  if (
    parsed.schemaVersion !== 1 ||
    parsed.status !== "prepared" ||
    parsed.invocation !== capture.invocation ||
    parsed.abortBeforeSubmit !== true ||
    parsed.outputDirectory !== capture.outputDirectory ||
    parsed.session === undefined ||
    !sameSession(parsed.session, capture.session)
  ) {
    throw new Error(
      "Pre-submit capture session manifest does not match the prepared capture identity",
    );
  }
};

export const prepareSignedTxPreSubmitCaptureDirectory = async (
  capture: SignedTxPreSubmitCapture,
): Promise<void> => {
  assertCaptureConfig(capture);
  await assertSignedTxPreSubmitCaptureSourceIdentity(capture);
  const parentPath = dirname(capture.outputDirectory);
  if ((await realpath(parentPath)) !== parentPath) {
    throw new Error(
      "Pre-submit capture parent directory must use its canonical physical path",
    );
  }
  await mkdir(capture.outputDirectory, { recursive: false, mode: 0o700 });
  await chmod(capture.outputDirectory, 0o700);
  if ((await realpath(capture.outputDirectory)) !== capture.outputDirectory) {
    throw new Error(
      "Pre-submit capture output directory did not resolve to its requested path",
    );
  }
  await writePrivateFileAtomically(
    captureSessionPath(capture),
    `${JSON.stringify(
      {
        schemaVersion: 1,
        status: "prepared",
        preparedAt: new Date().toISOString(),
        invocation: capture.invocation,
        abortBeforeSubmit: true,
        outputDirectory: capture.outputDirectory,
        session: capture.session,
      },
      null,
      2,
    )}\n`,
  );
  await syncDirectory(capture.outputDirectory);
  await syncDirectory(parentPath);
};

const assertBatchContext = (batch: SignedTxPreSubmitBatchContext): void => {
  const splitPathMatch = /^batch-(\d+)(?:\.[LR])*$/.exec(batch.splitPath);
  if (
    !Number.isSafeInteger(batch.ordinal) ||
    batch.ordinal < 0 ||
    !Number.isSafeInteger(batch.plannedBatchIndex) ||
    batch.plannedBatchIndex < 0 ||
    splitPathMatch === null ||
    splitPathMatch[1] !== batch.plannedBatchIndex.toString() ||
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
        !/^[0-9a-f]{56}$/.test(scriptHash) ||
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
        !/^[0-9a-f]{64}#\d+$/.test(outRef) ||
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

const assertInspectedTransactionValid = (
  tx: ReturnType<typeof inspectTransaction>,
  batch: SignedTxPreSubmitBatchContext,
  walletAddress: string,
): void => {
  if (!tx.canonicalCborRoundTrip) {
    throw new Error(
      "Captured signed transaction CBOR is not the canonical CML round-trip encoding",
    );
  }
  if (
    tx.vkeyWitnesses.length === 0 ||
    tx.vkeyWitnesses.some(
      ({ keyHash, signatureValid }) =>
        keyHash === undefined || signatureValid !== true,
    ) ||
    new Set(tx.vkeyWitnesses.map(({ keyHash }) => keyHash)).size !==
      tx.vkeyWitnesses.length
  ) {
    throw new Error(
      `Captured transaction vkey witness validation failed: ${tx.vkeyWitnesses
        .map(
          ({ location, keyHash, signatureValid, signatureError }) =>
            `${location}{key_hash=${keyHash ?? "missing"},valid=${String(signatureValid)},error=${signatureError ?? "none"}}`,
        )
        .join(",")}`,
    );
  }
  const declaredInputOutRefs = batch.inputs.map(({ outRef }) => outRef);
  if (
    new Set(tx.bodyInputOutRefs).size !== tx.bodyInputOutRefs.length ||
    tx.bodyInputOutRefs.length !== declaredInputOutRefs.length ||
    tx.bodyInputOutRefs.some((outRef) => !declaredInputOutRefs.includes(outRef))
  ) {
    throw new Error(
      `Captured transaction inputs do not match declared lineage: actual=[${tx.bodyInputOutRefs.join(",")}],declared=[${declaredInputOutRefs.join(",")}]`,
    );
  }
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
  const declaredOutputIndexes = new Set([
    ...expectedOutputs,
    ...batch.walletChangeOutputIndexes,
  ]);
  if (
    declaredOutputIndexes.size !== tx.bodyOutputAddresses.length ||
    tx.bodyOutputAddresses.some(
      (_, outputIndex) => !declaredOutputIndexes.has(outputIndex),
    )
  ) {
    throw new Error(
      "Captured transaction target and wallet-change outputs do not exactly partition the transaction body outputs",
    );
  }
  let walletAddressHex: string;
  let walletPaymentKeyHash: string;
  try {
    const paymentCredential =
      getAddressDetails(walletAddress).paymentCredential;
    if (paymentCredential?.type !== "Key") {
      throw new Error("wallet address does not have a key payment credential");
    }
    walletPaymentKeyHash = paymentCredential.hash;
    walletAddressHex = bytesToHex(
      CML.Address.from_bech32(walletAddress).to_raw_bytes(),
    );
  } catch (error) {
    throw new Error(
      `Captured transaction wallet address is invalid: ${String(error)}`,
    );
  }
  if (
    !tx.vkeyWitnesses.some(({ keyHash }) => keyHash === walletPaymentKeyHash)
  ) {
    throw new Error(
      "Captured transaction witness set does not contain the signing wallet payment key",
    );
  }
  if (
    batch.walletChangeOutputIndexes.some(
      (outputIndex) => tx.bodyOutputAddresses[outputIndex] !== walletAddressHex,
    )
  ) {
    throw new Error(
      "Captured transaction has a declared wallet-change output at a different address",
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
  await assertPreparedCaptureDirectory(capture);
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
  assertInspectedTransactionValid(tx, batch, walletAddress);
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
  for (let index = 0; index < payloads.length; index += 1) {
    await writePrivateFileAtomically(
      payloads[index]!.payloadPath,
      payloads[index]!.payload,
    );
  }
  await writePrivateFileAtomically(cborPath, bytes);
  // Keep the CBOR if metadata persistence fails: it is the primary forensic artifact.
  await writePrivateFileAtomically(
    metadataPath,
    `${JSON.stringify(metadata, null, 2)}\n`,
  );
  await syncDirectory(capture.outputDirectory);
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
  readonly walletAddress: string;
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

const readCaptureMetadata = async (
  path: string,
): Promise<{
  readonly metadata: PersistedCaptureMetadata;
  readonly sha256: string;
}> => {
  const bytes = await readPrivateRegularFile(path);
  const parsed = JSON.parse(bytes.toString("utf8")) as unknown;
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
  return {
    metadata: parsed as PersistedCaptureMetadata,
    sha256: createHash("sha256").update(bytes).digest("hex"),
  };
};

export const finalizeSignedTxPreSubmitCapture = async ({
  capture,
  expectedTargetNames,
}: {
  readonly capture: SignedTxPreSubmitCapture;
  readonly expectedTargetNames: readonly string[];
}): Promise<SignedTxPreSubmitCaptureComplete> => {
  assertCaptureConfig(capture);
  await assertPreparedCaptureDirectory(capture);
  await assertSignedTxPreSubmitCaptureSourceIdentity(capture);
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
  const metadataRecords = await Promise.all(
    metadataPaths.map(readCaptureMetadata),
  );
  const seenTxHashes = new Set<string>();
  const seenOrdinals = new Set<number>();
  const seenSplitPaths = new Set<string>();
  const targetCounts = new Map<string, number>();
  const expectedArtifactBasenames = new Set<string>([CAPTURE_SESSION_BASENAME]);
  const completeCaptures = [];
  for (let index = 0; index < metadataRecords.length; index += 1) {
    const { metadata: entry, sha256: metadataSha256 } = metadataRecords[index]!;
    const metadataPath = metadataPaths[index]!;
    expectedArtifactBasenames.add(basename(metadataPath));
    assertBatchContext(entry.batch);
    if (!sameSession(entry.session, capture.session)) {
      throw new Error(
        `Pre-submit capture session mismatch in ${metadataPaths[index]}`,
      );
    }
    if (
      !/^[0-9a-f]{64}$/.test(entry.txHash) ||
      entry.bodyHash !== entry.txHash ||
      !/^[0-9a-f]{64}$/.test(entry.signedTxSha256) ||
      typeof entry.walletAddress !== "string" ||
      basename(metadataPath) !== `signed-${entry.txHash}.cbor.json` ||
      seenTxHashes.has(entry.txHash) ||
      seenOrdinals.has(entry.batch.ordinal) ||
      seenSplitPaths.has(entry.batch.splitPath)
    ) {
      throw new Error(
        `Pre-submit capture has invalid or duplicate transaction identity in ${metadataPaths[index]}`,
      );
    }
    seenTxHashes.add(entry.txHash);
    seenOrdinals.add(entry.batch.ordinal);
    seenSplitPaths.add(entry.batch.splitPath);
    const expectedCborPath = join(
      capture.outputDirectory,
      `signed-${entry.txHash}.cbor`,
    );
    expectedArtifactBasenames.add(basename(expectedCborPath));
    if (entry.cborPath !== expectedCborPath) {
      throw new Error(
        `Pre-submit capture signed CBOR path mismatch for ${entry.txHash}`,
      );
    }
    const signedBytes = await readPrivateRegularFile(expectedCborPath);
    if (
      createHash("sha256").update(signedBytes).digest("hex") !==
      entry.signedTxSha256
    ) {
      throw new Error(
        `Pre-submit capture signed CBOR hash mismatch for ${entry.txHash}`,
      );
    }
    const reinspected = inspectTransaction(signedBytes.toString("hex"));
    if (reinspected.bodyHash !== entry.txHash) {
      throw new Error(
        `Pre-submit capture body hash changed before completion for ${entry.txHash}`,
      );
    }
    assertInspectedTransactionValid(
      reinspected,
      entry.batch,
      entry.walletAddress,
    );
    if (
      entry.payloads.length !== entry.batch.targets.length ||
      entry.outputs.referenceScripts.length !== entry.batch.targets.length ||
      entry.outputs.walletChange.length !==
        entry.batch.walletChangeOutputIndexes.length ||
      entry.batch.walletChangeOutputIndexes.some(
        (outputIndex) =>
          !entry.outputs.walletChange.some(
            (output) =>
              output.outputIndex === outputIndex &&
              output.outRef === `${entry.txHash}#${outputIndex.toString()}`,
          ),
      )
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
      expectedArtifactBasenames.add(basename(payload.payloadPath));
      const payloadBytes = await readPrivateRegularFile(payload.payloadPath);
      const reinspectedScript = reinspected.bodyScriptRefs.find(
        (candidate) =>
          inspectedScriptOutputIndex(candidate) === target.outputIndex,
      )!;
      if (
        payload.cmlType !== reinspectedScript.cmlType ||
        payload.languageTag !== reinspectedScript.languageTag ||
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
      metadataPath,
      metadataSha256,
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
  const unexpectedArtifacts = entries.filter(
    (entry) => !expectedArtifactBasenames.has(entry),
  );
  if (unexpectedArtifacts.length > 0) {
    throw new Error(
      `Pre-submit capture contains orphan or temporary artifacts: [${unexpectedArtifacts.join(",")}]`,
    );
  }
  const capturesByOrdinal = [...completeCaptures].sort(
    (left, right) => left.ordinal - right.ordinal,
  );
  const producedWalletChange = new Map<string, number>();
  for (const completeCapture of capturesByOrdinal) {
    for (const output of completeCapture.outputs.walletChange) {
      if (producedWalletChange.has(output.outRef)) {
        throw new Error(
          `Pre-submit capture declares duplicate wallet-change output ${output.outRef}`,
        );
      }
      producedWalletChange.set(output.outRef, completeCapture.ordinal);
    }
  }
  const consumedInputs = new Set<string>();
  for (const completeCapture of capturesByOrdinal) {
    for (const input of completeCapture.inputs) {
      if (consumedInputs.has(input.outRef)) {
        throw new Error(
          `Pre-submit capture spends input more than once: ${input.outRef}`,
        );
      }
      consumedInputs.add(input.outRef);
      const producerOrdinal = producedWalletChange.get(input.outRef);
      const inputTxHash = input.outRef.slice(0, 64);
      if (
        input.lineage === "synthetic_change" &&
        (producerOrdinal === undefined ||
          producerOrdinal >= completeCapture.ordinal)
      ) {
        throw new Error(
          `Pre-submit capture synthetic input does not reference prior captured wallet change: ${input.outRef}`,
        );
      }
      if (
        input.lineage === "live_seed" &&
        (producerOrdinal !== undefined || seenTxHashes.has(inputTxHash))
      ) {
        throw new Error(
          `Pre-submit capture live input incorrectly references captured output: ${input.outRef}`,
        );
      }
    }
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
    captures: capturesByOrdinal,
  };
  await writePrivateFileAtomically(
    completePath,
    `${JSON.stringify(complete, null, 2)}\n`,
  );
  await syncDirectory(capture.outputDirectory);
  return {
    schemaVersion: 1,
    status: "complete",
    completePath,
    expectedTargetCount: expectedTargetNames.length,
    captureCount: completeCaptures.length,
    targetNames: [...expectedTargetNames],
  };
};
