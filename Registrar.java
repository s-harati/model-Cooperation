package cooptest16summary;

public class Registrar {

	private AgentG agentG;
	private AgentU[] agentUArray;
	private int nLast, nSum, agentUCount; // are we using agentUCount here? yes: in update :/ // shall we calculate densities here too
	
	public Registrar(AgentG agentG, AgentU[] agentUArray, int agentUCount) {
		this.agentG = agentG;
		this.agentUArray = agentUArray;
		this.agentUCount = agentUCount;
		this.resetStats();
	}
	
	public int getNLast() {
		return this.nLast;
	}
	
	public int getNSum() {
		return this.nSum;
	}
	
	// this function is used in Exploring Starts algorithm
	public void setNLast(int startingNLast) {
		this.nLast = startingNLast;
	}
	
	// this function is used in Exploring Starts algorithm
	public void setNSum(int startingNSum) {
		this.nSum = startingNSum;
	}
	
	public void resetStats() {
		this.nLast = this.nSum = 0;
		for (AgentU agentU : agentUArray) { 	// added v5.0.0
			agentU.setDecision(0);				// this function is called at the beginning of each episode, hence no prior decision.
		}
	}
	
	public void step() {
		if (agentG.getSignal() == 0) {
			nSum += agentUCount;
		} else {
			// debugging
			//System.out.println("in registrar, before calling agentUs nLast:" + nLast);
						
			for (AgentU agentU : agentUArray) {
				agentU.makeDecision();
			}
			this.nLast = 0;
			for (AgentU agentU : agentUArray) {
				nLast += agentU.getDecision();
			}
			// debugging
			//System.out.println("in registrar, after calling agentUs nLast:" + nLast);
			nSum += nLast;
		}
	}
	

}
