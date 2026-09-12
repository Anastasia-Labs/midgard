import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import test from "node:test";
import { fileURLToPath } from "node:url";

const root = dirname(dirname(fileURLToPath(import.meta.url)));
const validator = join(root, "scripts/validate-custom-chain-config.sh");

const validGenesis = (override = {}) => ({
  slotLength: 1,
  activeSlotsCoeff: 0.05,
  securityParam: 2160,
  epochLength: 432000,
  protocolParams: { protocolVersion: { major: 11, minor: 0 } },
  ...override,
});

const validConfig = () => ({
  ShelleyGenesisFile: "/genesis/shelley-genesis.json",
  TestConwayHardForkAtEpoch: 0,
  ExperimentalHardForksEnabled: true,
});

const validate = ({
  genesis = validGenesis(),
  config = validConfig(),
} = {}) => {
  const runDir = mkdtempSync(
    join(tmpdir(), "midgard-phase4-config-validation-"),
  );
  try {
    writeFileSync(
      join(runDir, "shelley-genesis.json"),
      JSON.stringify(genesis),
    );
    writeFileSync(join(runDir, "config.json"), JSON.stringify(config));
    return spawnSync(
      "sh",
      [
        validator,
        join(runDir, "shelley-genesis.json"),
        join(runDir, "config.json"),
      ],
      { encoding: "utf8" },
    );
  } finally {
    rmSync(runDir, { recursive: true, force: true });
  }
};

test("custom Conway config accepts the verified Preprod major and consensus timing", () => {
  const result = validate();
  assert.equal(result.status, 0, result.stderr);
});

test("custom Conway config rejects a merely Plutus-V3-capable protocol major", () => {
  const result = validate({
    genesis: validGenesis({
      protocolParams: { protocolVersion: { major: 9, minor: 0 } },
    }),
  });
  assert.notEqual(result.status, 0);
  assert.match(result.stderr, /must match target protocol major 11; found 9/);
});

test("custom Conway config rejects the former five-minute forecast horizon", () => {
  const result = validate({
    genesis: validGenesis({ securityParam: 100, epochLength: 1000 }),
  });
  assert.notEqual(result.status, 0);
  assert.match(result.stderr, /securityParam must be 2160; found 100/);
});

for (const [label, override, expectedError] of [
  ["two-second slots", { slotLength: 2 }, /slotLength must be 1; found 2/],
  [
    "artificial deterministic block production",
    { activeSlotsCoeff: 1 },
    /activeSlotsCoeff must be 0.05; found 1/,
  ],
]) {
  test(`custom Conway config rejects ${label}`, () => {
    const result = validate({ genesis: validGenesis(override) });
    assert.notEqual(result.status, 0);
    assert.match(result.stderr, expectedError);
  });
}

test("custom Conway config rejects epoch length drift from 10k/f", () => {
  const result = validate({
    genesis: validGenesis({ epochLength: 270000 }),
  });
  assert.notEqual(result.status, 0);
  assert.match(result.stderr, /epochLength must be 432000; found 270000/);
});

test("custom Conway config rejects the artificial multi-day snapshot consensus", () => {
  const result = validate({
    genesis: validGenesis({ securityParam: 90000, epochLength: 900000 }),
  });
  assert.notEqual(result.status, 0);
  assert.match(result.stderr, /securityParam must be 2160; found 90000/);
});
