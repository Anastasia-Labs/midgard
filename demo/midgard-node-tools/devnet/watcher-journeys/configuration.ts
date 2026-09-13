import assert from "node:assert/strict";
import { readFile, writeFile } from "node:fs/promises";
import { join } from "node:path";

import preprod from "../preprod/configuration.json" with { type: "json" };

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
  const expectedGenesis = {
    startTime: genesis.systemStart,
    networkMagic: genesis.networkMagic,
    slotLength: { milliseconds: preprod.consensus.slotLength * 1000 },
    activeSlotsCoefficient: "1/20",
    epochLength: preprod.consensus.epochLength,
    securityParameter: preprod.consensus.securityParam,
    slotsPerKesPeriod: preprod.consensus.slotsPerKESPeriod,
    maxKesEvolutions: preprod.consensus.maxKESEvolutions,
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
