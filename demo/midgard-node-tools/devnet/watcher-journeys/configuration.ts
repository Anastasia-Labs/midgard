import assert from "node:assert/strict";
import { readFile, writeFile } from "node:fs/promises";
import { join } from "node:path";

import preprod from "../preprod/configuration.json" with { type: "json" };

const gcd = (a: bigint, b: bigint): bigint => (b === 0n ? a : gcd(b, a % b));

/** A plain decimal such as 0.25 in Ogmios's reduced-ratio spelling, "1/4". */
const ogmiosRatio = (decimal: number): string => {
  const match = /^(\d+)(?:\.(\d+))?$/.exec(String(decimal));
  assert(match, `Not a plain decimal: ${String(decimal)}`);
  const fraction = match[2] ?? "";
  const numerator = BigInt(match[1] + fraction);
  const denominator = 10n ** BigInt(fraction.length);
  const divisor = gcd(numerator, denominator);
  return `${numerator / divisor}/${denominator / divisor}`;
};

/** Compare the running ledger, including every cost-model entry, before spending. */
export async function verifyJourneyConfiguration(input: {
  runDirectory: string;
  ogmiosUrl: string;
}) {
  const query = async (method: string, params: object) => {
    const response = await fetch(input.ogmiosUrl, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({
        jsonrpc: "2.0",
        method,
        params,
        id: "configuration",
      }),
      signal: AbortSignal.timeout(10_000),
    });
    assert(
      response.ok,
      `Ogmios configuration query returned ${response.status}`,
    );
    const envelope = await response.json();
    assert.equal(envelope.error, undefined);
    return envelope.result;
  };
  const genesis = JSON.parse(
    await readFile(
      join(input.runDirectory, "genesis/shelley-genesis.json"),
      "utf8",
    ),
  );
  const captured = JSON.parse(
    await readFile(
      join(input.runDirectory, "config/preprod-configuration.json"),
      "utf8",
    ),
  );
  assert.deepEqual(
    captured,
    preprod,
    "Run was generated from a different Preprod profile",
  );
  const parameters = await query("queryLedgerState/protocolParameters", {});
  assert.deepEqual(
    parameters,
    preprod.ogmiosProtocolParameters,
    "Live protocol parameters differ from verified Preprod",
  );
  const liveGenesis = await query("queryNetwork/genesisConfiguration", {
    era: "shelley",
  });
  // generate.sh applies the isolated chain's consensus over Preprod's.
  const consensus = {
    ...preprod.consensus,
    ...preprod.isolatedChain.consensus,
  };
  const expectedGenesis = {
    startTime: genesis.systemStart,
    networkMagic: genesis.networkMagic,
    slotLength: { milliseconds: consensus.slotLength * 1000 },
    activeSlotsCoefficient: ogmiosRatio(consensus.activeSlotsCoeff),
    epochLength: consensus.epochLength,
    securityParameter: consensus.securityParam,
    slotsPerKesPeriod: consensus.slotsPerKESPeriod,
    maxKesEvolutions: consensus.maxKESEvolutions,
    maxLovelaceSupply: preprod.consensus.maxLovelaceSupply,
    updateQuorum: preprod.consensus.updateQuorum,
  };
  assert.deepEqual(
    Object.fromEntries(
      Object.keys(expectedGenesis).map((key) => [key, liveGenesis[key]]),
    ),
    expectedGenesis,
  );
  await writeFile(
    join(input.runDirectory, "work/verified-preprod-configuration.json"),
    `${JSON.stringify(
      {
        verifiedAt: new Date().toISOString(),
        source: preprod.source,
        genesis: expectedGenesis,
        parameters,
      },
      null,
      2,
    )}\n`,
  );
}
