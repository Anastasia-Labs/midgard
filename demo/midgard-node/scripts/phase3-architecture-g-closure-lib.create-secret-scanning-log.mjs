import { createHash } from "node:crypto";
import fs from "node:fs";
import path from "node:path";
import { Writable } from "node:stream";
import { finished } from "node:stream/promises";
import { StringDecoder } from "node:string_decoder";

import {
  assertRegularFile,
  execFileAsync,
  SHA256,
  sha256Bytes,
  sha256File,
} from "./phase3-architecture-g-closure-lib.evaluate-exact-closure-identity-shape.mjs";
import {
  containsSensitiveDriverOutput,
  MAX_RETAINED_LOG_LINE_CHARS,
  SECRET_LOG_REDACTION,
} from "./phase3-architecture-g-closure-lib.scan-submit-records.mjs";

/**
 * Retain driver diagnostics only after line-buffered secret scanning. Sensitive
 * and oversized lines are replaced before any bytes reach the evidence file.
 */
export const createSecretScanningLog = (filePath) => {
  if (!path.isAbsolute(filePath)) {
    throw new Error("secret-scanned log path must be absolute");
  }
  if (fs.existsSync(filePath)) {
    throw new Error(`refusing to overwrite ${filePath}`);
  }
  const descriptor = fs.openSync(
    filePath,
    fs.constants.O_CREAT | fs.constants.O_EXCL | fs.constants.O_WRONLY,
    0o600,
  );
  const decoder = new StringDecoder("utf8");
  let pending = "";
  let discardingOversizedLine = false;
  let sensitiveLineCount = 0;
  let oversizedLineCount = 0;
  let retainedLineCount = 0;
  let closed = false;

  const writeRetained = (value) => {
    fs.writeSync(descriptor, value);
  };
  const redact = ({ oversized = false } = {}) => {
    sensitiveLineCount += 1;
    if (oversized) oversizedLineCount += 1;
    writeRetained(`${SECRET_LOG_REDACTION}\n`);
  };
  const retainLine = (line, hasNewline) => {
    if (containsSensitiveDriverOutput(line)) {
      redact();
      return;
    }
    retainedLineCount += 1;
    writeRetained(hasNewline ? `${line}\n` : line);
  };
  const acceptText = (text) => {
    pending += text;
    while (true) {
      const newline = pending.indexOf("\n");
      if (discardingOversizedLine) {
        if (newline < 0) {
          pending = "";
          return;
        }
        pending = pending.slice(newline + 1);
        discardingOversizedLine = false;
        continue;
      }
      if (newline >= 0) {
        const line = pending.slice(0, newline).replace(/\r$/u, "");
        pending = pending.slice(newline + 1);
        if (line.length > MAX_RETAINED_LOG_LINE_CHARS)
          redact({ oversized: true });
        else retainLine(line, true);
        continue;
      }
      if (pending.length > MAX_RETAINED_LOG_LINE_CHARS) {
        pending = "";
        discardingOversizedLine = true;
        redact({ oversized: true });
      }
      return;
    }
  };

  const stream = new Writable({
    write(chunk, _encoding, callback) {
      try {
        acceptText(decoder.write(chunk));
        callback();
      } catch (error) {
        callback(error);
      }
    },
    final(callback) {
      try {
        acceptText(decoder.end());
        if (!discardingOversizedLine && pending.length > 0) {
          retainLine(pending.replace(/\r$/u, ""), false);
        }
        pending = "";
        fs.fsyncSync(descriptor);
        fs.closeSync(descriptor);
        closed = true;
        callback();
      } catch (error) {
        callback(error);
      }
    },
    destroy(error, callback) {
      if (!closed) {
        try {
          fs.closeSync(descriptor);
        } catch {
          // Preserve the original stream error.
        }
        closed = true;
      }
      callback(error);
    },
  });
  const completion = finished(stream).then(
    () => true,
    () => false,
  );

  return {
    stream,
    async complete() {
      if (!(await completion)) {
        throw new Error("secret-scanned log failed closed");
      }
      return {
        path: filePath,
        sha256: sha256File(filePath),
        bytes: fs.statSync(filePath).size,
        secretScan: {
          schemaVersion: "midgard-secret-scanned-log-v1",
          passed: sensitiveLineCount === 0,
          sensitiveLineCount,
          oversizedLineCount,
          retainedLineCount,
        },
      };
    },
  };
};

export const writeAtomicImmutableJson = (filePath, value) => {
  if (!path.isAbsolute(filePath))
    throw new Error("output path must be absolute");
  if (fs.existsSync(filePath))
    throw new Error(`refusing to overwrite ${filePath}`);
  const directory = path.dirname(filePath);
  fs.mkdirSync(directory, { recursive: true, mode: 0o700 });
  const temporaryPath = path.join(
    directory,
    `.${path.basename(filePath)}.${process.pid.toString()}.${Date.now().toString()}.tmp`,
  );
  const bytes = `${JSON.stringify(value, null, 2)}\n`;
  const descriptor = fs.openSync(
    temporaryPath,
    fs.constants.O_CREAT | fs.constants.O_EXCL | fs.constants.O_WRONLY,
    0o600,
  );
  try {
    fs.writeFileSync(descriptor, bytes);
    fs.fsyncSync(descriptor);
  } finally {
    fs.closeSync(descriptor);
  }
  try {
    fs.linkSync(temporaryPath, filePath);
  } finally {
    fs.unlinkSync(temporaryPath);
  }
  const directoryDescriptor = fs.openSync(directory, "r");
  try {
    fs.fsyncSync(directoryDescriptor);
  } finally {
    fs.closeSync(directoryDescriptor);
  }
};

export const normalizedImageId = (value) =>
  String(value ?? "").replace(/^sha256:/u, "");

export const capturePhase1CorpusIdentity = (phase1) => {
  const corpus = phase1?.corpus;
  const fields = [
    ["path", "corpusSha256", "Phase 1 corpus"],
    ["indexPath", "indexSha256", "Phase 1 corpus index"],
    ["manifestPath", "manifestSha256", "Phase 1 corpus manifest"],
  ];
  const identity = {};
  for (const [pathField, shaField, label] of fields) {
    const filePath = corpus?.[pathField];
    const expectedSha256 = corpus?.[shaField];
    if (
      typeof filePath !== "string" ||
      !path.isAbsolute(filePath) ||
      !SHA256.test(expectedSha256 ?? "")
    ) {
      throw new Error(`${label} identity is incomplete`);
    }
    const resolvedPath = path.resolve(filePath);
    assertRegularFile(resolvedPath, label);
    if (sha256File(resolvedPath) !== expectedSha256) {
      throw new Error(`${label} does not match its bound SHA-256`);
    }
    identity[pathField] = resolvedPath;
    identity[shaField] = expectedSha256;
  }
  if (
    typeof corpus?.sliceId !== "string" ||
    corpus.sliceId.trim().length === 0
  ) {
    throw new Error("Phase 1 corpus slice identity is incomplete");
  }
  const stressEnv = phase1?.stressCorpusEnv;
  if (
    stressEnv?.STRESS_CORPUS_PATH !== identity.path ||
    stressEnv?.STRESS_CORPUS_INDEX_PATH !== identity.indexPath ||
    stressEnv?.STRESS_CORPUS_MANIFEST_PATH !== identity.manifestPath ||
    stressEnv?.STRESS_CORPUS_SLICE_ID !== corpus.sliceId
  ) {
    throw new Error(
      "Phase 1 stress corpus environment diverges from its bound corpus",
    );
  }
  return { ...identity, sliceId: corpus.sliceId };
};

export const sourceIdentity = async (packageRoot) => {
  const root = path.resolve(packageRoot);
  const nodeExecutablePath = fs.realpathSync(process.execPath);
  const [
    { stdout: head },
    { stdout: status },
    { stdout: diff },
    { stdout: files },
  ] = await Promise.all([
    execFileAsync("git", ["rev-parse", "HEAD"], { cwd: packageRoot }),
    execFileAsync("git", ["status", "--porcelain=v1", "-z"], {
      cwd: packageRoot,
      encoding: "buffer",
      maxBuffer: 64 * 1024 * 1024,
    }),
    execFileAsync("git", ["diff", "--binary", "--no-ext-diff", "HEAD", "--"], {
      cwd: packageRoot,
      encoding: "buffer",
      maxBuffer: 128 * 1024 * 1024,
    }),
    execFileAsync(
      "git",
      ["ls-files", "--cached", "--others", "--exclude-standard", "-z"],
      {
        cwd: packageRoot,
        encoding: "buffer",
        maxBuffer: 64 * 1024 * 1024,
      },
    ),
  ]);
  const paths = files.toString("utf8").split("\0").filter(Boolean).sort();
  const tree = createHash("sha256");
  for (const relativePath of paths) {
    const absolutePath = path.resolve(root, relativePath);
    const name = Buffer.from(relativePath);
    let bytes;
    if (!fs.existsSync(absolutePath)) {
      bytes = Buffer.from("MIDGARD-SOURCE-MISSING-v1");
    } else {
      const stat = fs.lstatSync(absolutePath);
      if (stat.isSymbolicLink()) {
        bytes = Buffer.from(
          `MIDGARD-SOURCE-SYMLINK-v1:${fs.readlinkSync(absolutePath)}`,
        );
      } else if (stat.isFile()) {
        bytes = fs.readFileSync(absolutePath);
      } else {
        throw new Error(
          `source identity refuses non-file path ${relativePath}`,
        );
      }
    }
    const lengths = Buffer.allocUnsafe(12);
    lengths.writeUInt32LE(name.length, 0);
    lengths.writeBigUInt64LE(BigInt(bytes.length), 4);
    tree.update(lengths).update(name).update(bytes);
  }
  return {
    gitCommit: head.trim(),
    gitStatusSha256: sha256Bytes(status),
    trackedDiffSha256: sha256Bytes(diff),
    sourceTreeSha256: tree.digest("hex"),
    sourceTreeFileCount: paths.length,
    nodeVersion: process.version,
    nodeExecutablePath,
    nodeExecutableSha256: sha256File(nodeExecutablePath),
  };
};
