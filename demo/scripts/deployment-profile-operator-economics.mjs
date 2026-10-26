/** Operator amounts compiled into Aiken; validateProfile checks their integer shapes first. */
export const validateOperatorEconomics = (economics) => {
  if (
    BigInt(economics.requiredBondLovelace) !==
    BigInt(economics.slashingPenaltyLovelace) +
      BigInt(economics.fraudProverRewardLovelace)
  ) {
    throw new Error(
      "economics.requiredBondLovelace must equal economics.slashingPenaltyLovelace + economics.fraudProverRewardLovelace",
    );
  }
  if (
    economics.fraudProverRewardLovelace <
    economics.inactivitySlashingPenaltyLovelace
  ) {
    throw new Error(
      "economics.fraudProverRewardLovelace must be at least economics.inactivitySlashingPenaltyLovelace",
    );
  }
  if (
    economics.inactivitySlashingPenaltyLovelace >=
    economics.slashingPenaltyLovelace
  ) {
    throw new Error(
      "economics.inactivitySlashingPenaltyLovelace must be smaller than economics.slashingPenaltyLovelace",
    );
  }
};
