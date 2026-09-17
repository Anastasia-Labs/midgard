import assert from "node:assert/strict";
import { createHash } from "node:crypto";
import { mkdtempSync, mkdirSync, writeFileSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { test } from "node:test";
import { createHistoryArchiveDispatch } from "./history-archive-server.mjs";

const hash = "d441227553a0f1a965fee7d60a0f724b368dd1bddbc208730fccebcf";
const point = (
  blockNo,
  slot = blockNo * 20,
  blockHash = blockNo.toString(16).padStart(64, "0"),
) => ({
  blockNo: String(blockNo),
  slot: String(slot),
  blockHash,
  pointId: createHash("sha256")
    .update(`${slot}:${blockHash}:${blockNo}`)
    .digest("hex"),
});
const authority = {
  sourceId: "archive-a",
  operatorIdentitySha256: "a1".repeat(32),
  releaseFinality: {
    deploymentIdentityDigest: "d1".repeat(32),
    blueprintHash: "b1".repeat(32),
    policyDigest: "f1".repeat(32),
    policy: { confirmationDepth: 30 },
  },
};
const withArchive = async (action) => {
  const directory = mkdtempSync(join(tmpdir(), "native-archive-test-"));
  for (const path of ["canonical", "records", `native-scripts/${hash}`])
    mkdirSync(join(directory, path), { recursive: true });
  writeFileSync(join(directory, "canonical-ready"), '"generation"');
  for (let n = 1; n <= 30; n++)
    writeFileSync(
      join(directory, "canonical", `${n}.json`),
      JSON.stringify({
        point: point(n),
        prevHash: n === 1 ? "00".repeat(32) : point(n - 1).blockHash,
      }),
    );
  const record = {
    expectedScriptHash: hash,
    scriptBytesHex: "820180",
    publicationOutRef: `${"ab".repeat(32)}#0`,
    publicationOutputCbor: "8201",
    publicationTransactionBodyCbor: "a100",
    publicationTransactionIndex: 0,
    inclusionBlockTransactionIds: ["ab".repeat(32)],
    inclusionPoint: point(1),
    rawBlockCbor: "deadbeef",
  };
  writeFileSync(
    join(directory, "native-scripts", hash, "reference.json"),
    JSON.stringify(record),
  );
  const request = {
    deploymentIdentityDigest:
      authority.releaseFinality.deploymentIdentityDigest,
    blueprintHash: authority.releaseFinality.blueprintHash,
    finalityPolicyDigest: authority.releaseFinality.policyDigest,
    expectedScriptHash: hash,
    throughPoint: point(30),
  };
  try {
    await action({
      directory,
      record,
      request,
      dispatch: createHistoryArchiveDispatch(directory, authority),
    });
  } finally {
    rmSync(directory, { recursive: true, force: true });
  }
};

test("serves exact provider-bound native publication only at sparse-slot block finality", () =>
  withArchive(({ dispatch, record, request }) => {
    const result = dispatch(
      "POST",
      "/midgard/v1/native-script-publication",
      request,
    );
    assert.equal(result.status, 200);
    assert.equal(result.value.sourceId, authority.sourceId);
    assert.equal(
      result.value.operatorIdentitySha256,
      authority.operatorIdentitySha256,
    );
    assert.deepEqual(result.value.throughPoint, request.throughPoint);
    assert.equal(
      result.value.publicationOutputCbor,
      record.publicationOutputCbor,
    );
    assert.equal(result.value.rawBlockCbor, undefined);
    assert.equal(result.value.confirmationDepth, undefined);
    assert.equal(
      dispatch("POST", "/midgard/v1/native-script-publication", {
        ...request,
        throughPoint: point(29),
      }).status,
      404,
    );
    assert.equal(
      dispatch("POST", "/midgard/v1/native-script-publication", {
        ...request,
        blueprintHash: "cc".repeat(32),
      }).status,
      409,
    );
    assert.equal(
      dispatch("POST", "/midgard/v1/native-script-publication", {
        ...request,
        sourceId: "spoofed",
      }).status,
      400,
    );
    assert.equal(
      dispatch("POST", "/midgard/v1/native-script-publication", {
        ...request,
        throughPoint: { ...point(30), pointId: "00".repeat(32) },
      }).status,
      400,
    );
  }));

test("canonicality rejects gaps, rollback, changed boundaries and archive updates", () =>
  withArchive(({ directory, dispatch, request }) => {
    const query = { inclusionPoint: point(1), throughPoint: point(30) };
    assert.equal(
      dispatch(
        "POST",
        "/midgard/v1/native-script-publication/canonicality",
        query,
      ).value.canonical,
      true,
    );
    assert.equal(
      dispatch("POST", "/midgard/v1/native-script-publication/canonicality", {
        ...query,
        throughPoint: point(30, 600, "ff".repeat(32)),
      }).value.canonical,
      false,
    );
    rmSync(join(directory, "canonical-ready"));
    assert.equal(
      dispatch("POST", "/midgard/v1/native-script-publication", request).status,
      404,
    );
    writeFileSync(join(directory, "canonical-ready"), '"new-generation"');
    rmSync(join(directory, "canonical", "15.json"));
    assert.equal(
      dispatch(
        "POST",
        "/midgard/v1/native-script-publication/canonicality",
        query,
      ).value.canonical,
      false,
    );
    assert.equal(
      dispatch("POST", "/midgard/v1/native-script-publication", request).status,
      404,
    );
  }));

test("preserves historical payload GET and refuses foreign deployment lookups", () =>
  withArchive(({ directory, dispatch }) => {
    const header = "ab".repeat(28),
      value = { payloadEnvelopeCborHex: "0102" };
    writeFileSync(
      join(directory, "records", `${header}.json`),
      JSON.stringify(value),
    );
    assert.deepEqual(
      dispatch(
        "GET",
        `/midgard/v1/historical-payload/${authority.releaseFinality.deploymentIdentityDigest}/${header}`,
      ).value,
      value,
    );
    assert.equal(
      dispatch(
        "GET",
        `/midgard/v1/historical-payload/${"ff".repeat(32)}/${header}`,
      ).status,
      404,
    );
  }));
