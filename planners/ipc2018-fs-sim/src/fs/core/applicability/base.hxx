
#pragma once


namespace fs0 {

class State;
class GroundAction;
class GroundApplicableSet;

class ActionManagerI {
public:
	using ApplicableSet = GroundApplicableSet;

	virtual ~ActionManagerI() = default;

	virtual const std::vector<const GroundAction*>& getAllActions() const = 0;

	//! Return the set of all actions applicable in a given state
	virtual ApplicableSet applicable(const State& state, bool enforce_state_constraints) const = 0;

	//! Return whether the given action is applicable in the given state
	virtual bool applicable(const State& state, const GroundAction& action, bool enforce_state_constraints) const = 0;

	//! Whether the action-whitelist generated by the particular type of action manager
	//! contains actions which are guaranteed to be applicable or not
	//! By default, we assume they are not.
	virtual bool whitelist_guarantees_applicability() const { return false; }
};

} // namespaces
