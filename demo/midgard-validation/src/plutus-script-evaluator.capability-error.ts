/** A missing/incompatible evaluator is infrastructure failure, never a script verdict. */
export class MidgardUplcCapabilityError extends Error {
  constructor(detail: string, options?: ErrorOptions) {
    super(detail, options);
    this.name = "MidgardUplcCapabilityError";
  }
}
