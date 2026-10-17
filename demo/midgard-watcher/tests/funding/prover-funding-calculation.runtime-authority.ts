import { createWatcherProtocolParameterRuntimeAuthority } from "../../src/funding/prover-funding.js";
import { makeWatcherDeploymentAuthorityFixture } from "../support/deployment-authority-fixture.js";
import { ledgerParameterQuery } from "../support/ledger-protocol-parameters.js";

export const runtimeAuthority = async (
  deploymentIdentity: ReturnType<
    typeof makeWatcherDeploymentAuthorityFixture
  >["result"],
  minFeeCoefficient = 44,
  maxCollateralInputs = 3,
  collateralPercentage = 150,
) =>
  await createWatcherProtocolParameterRuntimeAuthority({
    deploymentIdentity,
    query: ledgerParameterQuery({
      minFeeA: BigInt(minFeeCoefficient),
      collateralPercentage: BigInt(collateralPercentage),
      maxCollateralInputs: BigInt(maxCollateralInputs),
    }),
  });
