import { vi } from "vitest";

import { unsafeCreateWatcherProtocolParameterRuntimeAuthorityForTest } from "../../src/funding/prover-funding.js";
import { makeWatcherDeploymentAuthorityFixture } from "../support/deployment-authority-fixture.js";
import { ogmiosParameters } from "./prover-funding-calculation.transaction-cbor.js";

export const runtimeAuthority = async (
  deploymentIdentity: ReturnType<
    typeof makeWatcherDeploymentAuthorityFixture
  >["result"],
  minFeeCoefficient = 44,
  maxCollateralInputs = 3,
  collateralPercentage = 150,
) =>
  await unsafeCreateWatcherProtocolParameterRuntimeAuthorityForTest({
    deploymentIdentity,
    ogmiosUrl: "http://127.0.0.1:1337",
    timeoutMs: 10_000,
    fetchImpl: vi.fn(async (_url, init) => {
      const request = JSON.parse(String(init?.body)) as {
        readonly id: string;
      };
      return new Response(
        JSON.stringify({
          jsonrpc: "2.0",
          id: request.id,
          result: {
            ...ogmiosParameters(),
            minFeeCoefficient,
            maxCollateralInputs,
            collateralPercentage,
          },
        }),
        { status: 200, headers: { "content-type": "application/json" } },
      );
    }) as unknown as typeof fetch,
  });
