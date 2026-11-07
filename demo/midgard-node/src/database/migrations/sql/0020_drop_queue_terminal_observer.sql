-- State-queue terminal transitions (merges and removals) are a temporal
-- projection over the L1 follower's facts (`node_l1_queue_terminals`, N4):
-- the follower derives them in the block that lands them and its generated
-- rewind removes them on a rollback. The correction observer that recorded
-- them from Kupo and Ogmios at confirmation depth is deleted, so nothing
-- writes or reads its state or the outcomes it admitted.
DROP TABLE da_payload_terminal_outcomes;
DROP TABLE state_queue_terminal_observer_states;
