/** Rebuilding a transaction body with some of its fields replaced. */
import { CML } from "@lucid-evolution/lucid";

/** `body` with its inputs, outputs and fee as given, every other field
 * kept. */
export const rebodied = (
  body: CML.TransactionBody,
  {
    inputs = body.inputs(),
    outputs = body.outputs(),
    fee = body.fee(),
  }: {
    readonly inputs?: CML.TransactionInputList;
    readonly outputs?: CML.TransactionOutputList;
    readonly fee?: bigint;
  },
): CML.TransactionBody => {
  const next = CML.TransactionBody.new(inputs, outputs, fee);
  const keep = <T>(value: T | undefined, set: (value: T) => void) => {
    if (value !== undefined) set(value);
  };
  keep(body.ttl(), (value) => next.set_ttl(value));
  keep(body.certs(), (value) => next.set_certs(value));
  keep(body.withdrawals(), (value) => next.set_withdrawals(value));
  keep(body.auxiliary_data_hash(), (value) =>
    next.set_auxiliary_data_hash(value),
  );
  keep(body.validity_interval_start(), (value) =>
    next.set_validity_interval_start(value),
  );
  keep(body.mint(), (value) => next.set_mint(value));
  keep(body.script_data_hash(), (value) => next.set_script_data_hash(value));
  keep(body.collateral_inputs(), (value) => next.set_collateral_inputs(value));
  keep(body.required_signers(), (value) => next.set_required_signers(value));
  keep(body.network_id(), (value) => next.set_network_id(value));
  keep(body.collateral_return(), (value) => next.set_collateral_return(value));
  keep(body.total_collateral(), (value) => next.set_total_collateral(value));
  keep(body.reference_inputs(), (value) => next.set_reference_inputs(value));
  keep(body.voting_procedures(), (value) => next.set_voting_procedures(value));
  keep(body.proposal_procedures(), (value) =>
    next.set_proposal_procedures(value),
  );
  keep(body.current_treasury_value(), (value) =>
    next.set_current_treasury_value(value),
  );
  keep(body.donation(), (value) => next.set_donation(value));
  return next;
};
