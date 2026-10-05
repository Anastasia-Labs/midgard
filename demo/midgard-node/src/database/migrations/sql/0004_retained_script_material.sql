CREATE TABLE public.cek_program_material_retained_state_owners (
    header_hash bytea NOT NULL,
    program_envelope_hash bytea NOT NULL,
    material_root bytea NOT NULL,
    PRIMARY KEY (header_hash, program_envelope_hash, material_root),
    FOREIGN KEY (header_hash) REFERENCES public.da_payloads(header_hash) ON DELETE CASCADE,
    FOREIGN KEY (program_envelope_hash, material_root)
      REFERENCES public.cek_program_material_memberships(program_envelope_hash, material_root) ON DELETE RESTRICT
);
