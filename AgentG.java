package cooptest17;

import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.Random;
import java.util.concurrent.ThreadLocalRandom;

public class AgentG {

	static int indexVarIndependent = 0, indexVarResponse = 1; // v3.0.0

	private Registrar registrar;
	private String algorithm;
	private int agentUCount;
	private int stepsPerEpisode, episodesPerRun;
	private int numLevelsVarIndependent, numLevelsVarResponse;
	private double gamma;
	// private double[] policy; // 1: decided for policy to be probability for each
	// action in each state. 2: since numActions==2, one dimension is enough. 3: for
	// state #i, i define policy[i] as the probability of recommending signal=1 as
	// action.
	private double[][] policy; // v3.0.0
	private Episode episode;
	private double epsilon, alpha; // v3.0.0
	// private int numStates; // v3.0.0
	private int numActions = 2; // in this project only binary actions are assumed. FOR ANY FUZZY DESIGN THE
								// WHOLE CODE SHOULD BE REVIZED!
	// private int /*state,*/ stateVarIndependent, stateVarResponse; // version
	// 3.0.0
	private int[] state; // version 3.0.0
	private int signal; // this is 'action' in RL terminology. don't ask why it's not called that here.
						// note: comments should be concise and relevant.
	private double targetCoopRatio = 0.5; // if the ratio of cooperating AgentU's reaches this value then the operation
											// ends with success.
	private double[][][] Q; // v3.0.0

	public AgentG(/*String algorithm, */int agentUCount, int stepsPerEpisode, int episodesPerRun,
			int numLevelsVarIndependent, int numLevelsVarResponse, double gamma, double epsilon, double alpha) {

//		this.algorithm = algorithm; // v4.0.0
		this.agentUCount = agentUCount;
		this.stepsPerEpisode = stepsPerEpisode;
		this.episodesPerRun = episodesPerRun;
		this.numLevelsVarIndependent = numLevelsVarIndependent;
		this.numLevelsVarResponse = numLevelsVarResponse;
		this.gamma = gamma;

		// this.numStates = numLevelsVarIndependent * numLevelsVarResponse; // including
		// target state // v3.0.0
		// this.policy = new double[numStates]; // v3.0.0
		this.policy = new double[numLevelsVarIndependent][numLevelsVarResponse]; // v3.0.0
		this.episode = new Episode(stepsPerEpisode);
		// debugging
		// for(int i = 0 ; i < stepsPerEpisode ; i++) {System.out.print(episode.R[i]);}
		// System.out.println();
		this.state = new int[2]; // version 3.0.0
		this.Q = new double[numLevelsVarIndependent][numLevelsVarResponse][numActions]; // v3.0.0
		this.epsilon = epsilon;
		this.alpha = alpha;
	}

	public void setRegistrar(Registrar registrar) {
		this.registrar = registrar;
	}
	
	public void setAlgorithm(String algorithm) { // v4.0.0
		this.algorithm = algorithm;
	}

	/*
	 * // this function is used in validation dataset preparation, through RunMaker
	 * public void setSignal(int signal) { this.signal = signal; }
	 */

	public int getSignal() {
		return this.signal;
	}

	public double[][] getPolicy() { // updated v3.0.0
		return this.policy;
	}

	public double[] getRewards() {
		return this.episode.R;
	}

	public int[][] getStates() { // updated v3.0.0
		return this.episode.S;
	}

	public int[] getActions() {
		return this.episode.A;
	}

	public void runCalibrationAlgorithm() {
		// test & debug :
		// System.out.println("in AgentG.runAlgorithm()");
		// System.out.println(algorithm);
		// if (algorithm == "MonteCarloES_FirstVisit") {System.out.println("check1");}
		// if (2+2 == 4) {System.out.println("2+2=4");}
		// String s1 = algorithm;
		// String s2 = "MonteCarloES_FirstVisit";
		// if (s1==s2) {System.out.println("s1=s2");} else
		// {System.out.println("s1!=s2"); System.out.println(s1);
		// System.out.println(s2);}
		// System.out.println(s1.length());
		// System.out.println(s2.length());
		// if (Objects.equals(s1, s2)) {System.out.println("Ah!");}
		// if (algorithm == s1) {System.out.println("algorithm=s1");}
		// if (algorithm == "MonteCarloES_EveryVisit") {System.out.println("check2");}
		if (Objects.equals(algorithm, "MonteCarlo_offPolicy")) {
			//System.out.println("check3"); // ok
			runMC_offPolicy();
		} else if (Objects.equals(algorithm, "MonteCarlo_onPolicy")) {
			runMC_onPolicy();
		} else if (Objects.equals(algorithm, "TD_SARSA")) {
			runTD_SARSA();
		} else if (Objects.equals(algorithm, "TD_QLearning")) {
			runTD_QLearning();
			//testargmaxQ(); //OK
		} else if (Objects.equals(algorithm, "TD_expectedSARSA")) {
			runTD_expectedSARSA();
		} else if (Objects.equals(algorithm, "TD_DoubleQLearning")) {
			runTD_DoubleQLearning();
		} else if (Objects.equals(algorithm, "TD_DoubleSARSA")) {
			runTD_DoubleSARSA();
		} else if (Objects.equals(algorithm, "TD_DoubleExpectedSARSA")) {
			runTD_DoubleExpectedSARSA();
		} 
		// System.out.println("still in runAlgorithm");
	}

	public void runBaseline_Random(String outRefFileName) throws IOException {
		System.out.println("***starting runBaseline_Random()***");
		int nLast,nSum;
		double coopRatio;
		// loop
		for (int epi = 0 ; epi < this.episodesPerRun ; epi++) {
			episode.reset();
			registrar.resetStats();
			nLast = registrar.getNLast();
			nSum = registrar.getNSum();
			state[indexVarIndependent] = 0;
			state[indexVarResponse] = 0;
			episode.S[indexVarIndependent][0] = state[indexVarIndependent];
			episode.S[indexVarResponse][0] = state[indexVarResponse];
			episode.R[0] = -1;
			// inner loop
			for (int timeStep=0 ; timeStep < stepsPerEpisode-1 ; timeStep++) {
				signal = (Math.random() > 0.5)? 1 : 0 ;
				System.out.println("in G.runBaseline_Random()  t="+timeStep+"  signal="+signal); // debugging
				System.out.println("   before registrar.setp() : nLast="+nLast+"  nSum="+nSum); // debugging
				registrar.step();
				nLast = registrar.getNLast();
				nSum = registrar.getNSum();
				System.out.println("back in G.runBaseline_Random()"); // debugging
				System.out.println("   after registrar.setp() : nLast="+nLast+"  nSum="+nSum); // debugging
				System.out.println("   checking: registrar.nLast="+registrar.getNLast()+"  registrar.nSum="+registrar.getNSum());
				coopRatio = 1.0 * nLast / agentUCount;
				System.out.println("coopRatio="+coopRatio);
				//setState(nSum, nLast, timeStep);
				episode.A[timeStep] = signal;
				episode.S[indexVarIndependent][timeStep+1] = nLast;//state[indexVarIndependent];
				episode.S[indexVarResponse][timeStep+1] = agentUCount;//state[indexVarResponse];
				episode.R[timeStep+1] = (coopRatio >= targetCoopRatio)? 0 : coopRatio - 1;
				if (coopRatio >= targetCoopRatio) {break;}			
			} // end of inner loop
			
			// saving rewards in output file
			try {
				writeArrayToFile(episode.R, (outRefFileName + "random.txt"));
			} catch (IOException e) {
				// TODO Auto-generated catch block
				System.out.println("Problem!");
				e.printStackTrace();
			}
		} // end of outer loop
		System.out.println("***ending runBaseline_Random()***");
	} // end of method runBaseline_Random()

	/*public void runPrediction(int startingState, double[] givenPolicy) { // desinged for MC // to be revised for v3.0.0
		state = startingState;
		policy = givenPolicy;
		registrar.resetStats();
		signal = (Math.random() < policy[state]) ? 1 : 0;
		generateEpisode(0);
	}*/

	public void runValidation(String outRefFileName) throws IOException {
		// int signal;
		double refLastReward, refSumReward, refMeanReward;
		StringBuilder refRewardString = new StringBuilder();
		//long numAllPolicies = (long) Math.pow(2, stepsPerEpisode-1);
		long numAllPolicies = (long) Math.pow(2, stepsPerEpisode);
		// debugging
		long[] testi = {2047,2048,4095,4096};
		//for (long i = 0; i < numAllPolicies; i++) {
		for (long i : testi) {	
			//String policyString = String.format("%" + (stepsPerEpisode-1) + "s", Long.toBinaryString(i)).replaceAll(" ",
			//		"0"); // binary string of length stepsPerEpisode
			String policyString = String.format("%" + (stepsPerEpisode) + "s", Long.toBinaryString(i)).replaceAll(" ",
					"0"); // binary string of length stepsPerEpisode
			System.out.println("runValidation..i="+i+" , policyString="+policyString); // debugging
			registrar.resetStats();
			refSumReward = 0;
			refRewardString.setLength(0); // clearing the StringBuilder
			System.out.println("runValidation..refRewardString="+refRewardString); // debugging
			double coopRatio = 0.0; // v5.1.0
			refLastReward = coopRatio - 1;
			refSumReward += refLastReward;
			refRewardString.append(refLastReward + " ");
			for (int t = 0; t < stepsPerEpisode-1; t++) {
			//for (int t = 0; t < stepsPerEpisode; t++) {
//				refLastReward = coopRatio - 1;
//				refSumReward += refLastReward;
//				refRewardString.append(refLastReward + " ");
				
				signal = (policyString.charAt(t) == '1' ? 1 : 0);
				System.out.println("runValidation..inner loop t="+t+" , signal="+signal+" , nextreward="+refLastReward); // debugging
				// agentG.setSignal(signal);
				registrar.step();
				int nLast = registrar.getNLast(); // int nSum = registrar.getNSum();
				coopRatio = 1.0 * nLast / agentUCount;
				if (coopRatio >= targetCoopRatio) {
					break;
				}
				// v5.1.0 these lines were moved to the beginning of the loop so that order of rewards in timesteps matches that of RL sim episodes
				refLastReward = coopRatio - 1;
				refSumReward += refLastReward;
				refRewardString.append(refLastReward + " ");
			}
			/*
			 * int nLast = registrar.getNLast(); int nSum = registrar.getNSum(); double
			 * coopRatio = 1.0 * nLast / agentUCount ; refLastReward = coopRatio - 1 ;
			 */
			refMeanReward = refSumReward / stepsPerEpisode;
			// save all possible rewards (as validation reference)
			try {
				FileWriter sumsFileWriter = new FileWriter(outRefFileName + ".txt", true);
				sumsFileWriter.write(refMeanReward + " ");
				sumsFileWriter.close();
				FileWriter fullFileWriter = new FileWriter(outRefFileName + "full.txt", true);
				fullFileWriter.write(refRewardString + "\n");
				fullFileWriter.close();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
	}
	
	private int getArgmaxQ(int s0, int s1) { // added in v3.1.0 // s0: independent , s1:dependent
		int argmax = 0;
		for (int i=0 ; i<numActions ; i++) {
			argmax = (Q[s0][s1][i] > Q[s0][s1][argmax])? i : argmax;
		}
		return breakTiesRandomly(Q[s0][s1] , Q[s0][s1][argmax]);
	}
	
	private int getArgmaxQ(int s0, int s1, double[][][] Q) { // s0: independent , s1:dependent
		int argmax = 0;
		for (int i=0 ; i<numActions ; i++) {
			argmax = (Q[s0][s1][i] > Q[s0][s1][argmax])? i : argmax;
		}
		return breakTiesRandomly(Q[s0][s1] , Q[s0][s1][argmax]);
	}

	private int getArgmax(double[] vec) {
		int argmax = 0;
		for (int i=0 ; i<vec.length ; i++) {
			argmax = (vec[i] > vec[argmax])? i : argmax;
		}
		return breakTiesRandomly(vec , vec[argmax]);
	}
	
	private void testargmaxQ() { // OK
		// set Q
		for (int i=0 ; i < numLevelsVarIndependent ; i++) {
			for (int j=0 ; j < numLevelsVarResponse ; j++) {
				for (int k=0 ; k < numActions ; k++) {
					Q[i][j][k] = Math.abs(100*i + 10*j + k - 100);
					System.out.print("Q["+i+"]["+j+"]["+k+"]="+Q[i][j][k]+"    ");
				}
				System.out.println();
			}
		}
		// test
		int resTemp;
		for (int i=0 ; i < numLevelsVarIndependent ; i++) {
			for (int j=0 ; j < numLevelsVarResponse ; j++) {
				resTemp = getArgmaxQ(i,j);
				System.out.print("argmaxQ("+i+","+j+")="+resTemp+"    ");
				System.out.println("Q["+i+"]["+j+"][argmaxQ]="+Q[i][j][resTemp]);
			}
		}
	} 

	/*private void runMC_ES() { // First-Visit & Every-Visit MonteCarlo with Exploring Starts // DISCARDED
		// test & debug : considering removal of the ES algorithm
		// System.out.println("in AgentG.runMC_ES()");
		// initializations
		// initialize policy (arbitrary) as of v2.2.9 initial probability values of
		// policy are NOT set randomly;
		// rather, they are set to give equal chance for all actions.
		for (int i = 0; i < policy.length; i++) {
			policy[i] = 1 / numActions; // replaced Math.random();
			// test & debug : 0 policy
			// System.out.print(policy[i] + "\t"); // ok
		}
		// initialize Q (arbitrary) // range of values not clear... let's initialize at
		// 0
		double[][] Q = new double[numStates][numActions]; // java default value: 0.0 ... v230 how about random
															// assignments?
		for (int i = 0; i < numStates; i++) {
			for (int j = 0; j < numActions; j++) {
				Q[i][j] = -Math.random();
			}
		}
		// initialize Returns (empty list)
		List<Double>[][] Returns = new List[numStates][numActions]; // a 2D List (of ArrayLists, as defined in the
																	// following line)
		for (int i = 0; i < numStates; i++) {
			for (int j = 0; j < numActions; j++) {
				Returns[i][j] = new ArrayList<Double>();
			}
		}

		// loop(episodesPerRun)
		for (int epi = 0; epi < episodesPerRun; epi++) {
			
			 * // choose S0,A0 (exploring starts) int startingT =
			 * ThreadLocalRandom.current().nextInt(1, stepsPerEpisode + 1); // inclusive of
			 * minimum and exclusive of maximum
			 * //System.out.println("startintT: "+startingT+" stepsPerEpisode: "
			 * +stepsPerEpisode); // debugging int startingNLast =
			 * ThreadLocalRandom.current().nextInt(0, (int) (agentUCount*targetCoopRatio));
			 * //System.out.println("startintNLast: "+startingNLast+" agentUCount: "
			 * +agentUCount); // debugging int startingNSum = startingNLast +
			 * ThreadLocalRandom.current().nextInt(0, agentUCount*(startingT - 1) + 1);
			 * //System.out.println("startingNSum: "+startingNSum+" maximum: "+(agentUCount*
			 * startingT)); // debugging int timeStepAdjusted = 0+startingT+1;
			 * calculateStateVars(startingNLast,startingNSum,timeStepAdjusted);
			 
			int startingT = 0; // debugging
			state = 0; // v2.3.1 the starting state is always zero. we try different starting signals
			signal = (int) (Math.random() * numActions); // parentheses added in debugging: 0 was always the result

			// System.out.println("Episode " + epi + " ; starting state,signal: " + state +
			// "," + signal);
			// generate episode from current state and action (or signal), following policy
			registrar.resetStats(); // added in debugging: nSum was loose and growing!
			// System.out.println("line127ok");
			generateEpisode(startingT);
			// System.out.println("line129ok");
			// test&debugging
			// System.out.println("in agentG, episode" + epi);
			// for(int x = 0 ; x < stepsPerEpisode ; x++) {System.out.print(x + "," +
			// episode.S[x] + " ");} // up until here the values in R[] are all there
			// System.out.println();

			// gain G = 0
			double G = 0;
			// visits table (auxiliary) initially set at 0 for all state-action pairs
			int[][] visits = new int[numStates][numActions];
			// loop(stepsPerEpisode from last to first)
			for (int t = stepsPerEpisode - 1; t >= 0; t--) { // System.out.println("re-reading episode"+epi+" step"+t);
																// // debugging
				// gain G = gamma*G + R[t+1] // note that Sutton's book's indices for R are 1
				// up.
				G = gamma * G + episode.R[t];

				// v2.3.0: steps are being read backwards. if the current step is in target
				// state then
				// no action is needed, and no analysis is needed. The visits and Q arrays have
				// no room for this state.
				// in such cases, the loop continues to the next iteration.
				if (episode.S[t] == numStates) {
					continue;
				}

				// update auxiliary visits table
				visits[episode.S[t]][episode.A[t]] += 1;
				// debugging:
				// System.out.print("visits to this pair: ");
				// System.out.println(visits[episode.S[t]][episode.A[t]]);
				// if (we are running the EveryVisit algorithm) or (we are running FirstVisit &
				// this is the FIRST VISIT for pair S[t],A[t]) : update Q
				if (Objects.equals(algorithm, "MonteCarloES_EveryVisit") || (visits[episode.S[t]][episode.A[t]] == 1)) {
					// debuging
					// System.out.print("Aah! here we go: ");
					// System.out.println(visits[episode.S[t]][episode.A[t]]);
					// append G to Returns(S[t],A[t])
					Returns[episode.S[t]][episode.A[t]].add(G);
					// Q(S[t],A[t]) = average(Returns(S[t],A[t])
					double sumReturns = 0; // temp aux var
					for (double x : Returns[episode.S[t]][episode.A[t]]) {
						sumReturns += x;
					}
					Q[episode.S[t]][episode.A[t]] = sumReturns / Returns[episode.S[t]][episode.A[t]].size();
					// policy(S[t]) = argmax.a Q(S[t],a) // note: to define policy as probability,
					// look at Q[S[t]][all A's], and distribute the probabilities accordingly.
					// reminder: here A = signal.
					// note: exception: early success leads to high rewards of 0 in the end of
					// episode. they may cause division by zero in calculation of policy!
					if (Q[episode.S[t]][0] + Q[episode.S[t]][1] != 0.0) {
						// Note: Q values are average Returns, which are gains, which are weighted
						// rewards, which are negative!
						// That's why we're using Q(s,0) in numerator.
						policy[episode.S[t]] = Math.abs(Q[episode.S[t]][0])
								/ (Math.abs(Q[episode.S[t]][0]) + Math.abs(Q[episode.S[t]][1])); // reminder: here
																									// policy[s] is
																									// p[S=s,A=1]
					} // otherwise, the value of policy[S[t]] is not updated in this t. ... or... set
						// to 0.5?
						// else {
						// policy[episode.S[t]] = 1 / numActions; // this is because in this particular
						// case, both actions must have lead to reward 0, meaning they are both equally
						// advisable.
						// }
				}
			}
		}
	}*/

	// auxiliary function to generate episodes. uses policy. uses and changes state
	// and signal. writes to episode.
	/*private void generateEpisode(int startingT) {
		for (int t = 0; t < stepsPerEpisode; t++) {
			episode.R[t] = 0; // initializing rewards at maximum, so that a 'break' leads to maximum sum of
								// rewards
			episode.A[t] = 1;
			episode.S[t] = numLevelsVarIndependent * numLevelsVarResponse; // target state. if episode is broken midway,
																			// the untaken steps are assumed to be in
																			// target state.
		}
		for (int t = 0; t < stepsPerEpisode; t++) {
			episode.S[t] = state;
			episode.A[t] = signal;
			registrar.step(); // signal is given to AgentU's and their response recorded.
			int nLast = registrar.getNLast();
			int nSum = registrar.getNSum();
			double coopRatio = 1.0 * nLast / agentUCount;
			// System.out.println("in agentG, t=" + t + " nLast=" + nLast + " nSum=" + nSum
			// + " coopRatio=" + coopRatio);
			if (coopRatio >= targetCoopRatio) {
				break;
			} // note: best reward is 0, which is the default value of array, so to simply
				// break is rewarding.
			episode.R[t] = coopRatio - 1; // reward is negative. note that in Sutton's book this is called time t+1.
			// System.out.println("t=" + t + " R=" + (episode.R[t])); // test&debugging //
			// R[] is filled completely here
			int timeStepAdjusted = t + startingT + 1;
			calculateStateVars(nLast, nSum, timeStepAdjusted);
			signal = (Math.random() < policy[state]) ? 1 : 0; // new signal. reminder: policy is the probability of
																// recommending signal=1 for each state.
			// debugging
			// System.out.println("in generateEpisode() : " + "S[" + (t+1) + "]=" + state +
			// " ; A[" + (t+1) + "]=" + signal + " ; R[" + t + "]=" + episode.R[t]);
		} // note that at the end of the loop, the state and signal are changed but are
			// not written in episode.
	}*/
	
	private void generateEpisode(int s0, int s1, double[][] b) { // starting from state (s0,s1) and following policy b
		int nLast, nSum;
		double coopRatio;
		registrar.resetStats(); // elementary, my dear watson
		episode.S[0][0] = s0;
		episode.S[1][0] = s1;
		for (int timeStep = 1; timeStep < stepsPerEpisode; timeStep++) {
//			// debugging
//			System.out.println("debugging generateEpisode(int s0, int s1, double[][] b)");
//			System.out.println("timeStep="+timeStep);
//			//
				// action
			updateSignal(b);
			episode.A[timeStep-1] = signal;
			registrar.step(); // signal is given to AgentU's and their response recorded.
			nLast = registrar.getNLast();
			nSum = registrar.getNSum();
			coopRatio = 1.0 * nLast / agentUCount;
			if (coopRatio >= targetCoopRatio) {
				break;
			} 
				// reward
			episode.R[timeStep] = coopRatio - 1;
				// state
			setState(nSum, nLast, timeStep);
			episode.S[indexVarIndependent][timeStep] = state[indexVarIndependent];
			episode.S[indexVarResponse][timeStep] = state[indexVarResponse];
//			// debugging
//			System.out.println("nSum="+nSum+"  nLast="+nLast);
//			System.out.print("S_ind_t="+episode.S[indexVarIndependent][timeStep]);
//			System.out.println(" S_res_t="+episode.S[indexVarResponse][timeStep]);
//			System.out.println("end of debugging generateEpisode(int s0, int s1, double[][] b)");
//			//
		}
	}
	
	/*
	 * private void calculateStateVars(int nLast, int nSum, int timeStepAdjusted) {
	 * double coopRatio = 1.0 * nLast / agentUCount ; stateVarResponse = (int) (1.0
	 * * numLevelsVarResponse * coopRatio / targetCoopRatio) ; stateVarIndependent =
	 * (agentUCount*(timeStepAdjusted) == nSum) ? numLevelsVarIndependent - 1 :
	 * (int) (1.0 * numLevelsVarIndependent * nSum / (agentUCount *
	 * timeStepAdjusted)) ; state = stateVarIndependent + numLevelsVarIndependent *
	 * stateVarResponse; // new state }
	 */

	// v3.0.0 re-defining state as a pair of independent and response variables
	private void setState(int nSum, int nLast, int timeStep) {
		double coopRatio = 1.0 * nLast / agentUCount;
		state[indexVarIndependent] = (nSum == agentUCount * timeStep) ? numLevelsVarIndependent - 1
				: (int) (1.0 * (numLevelsVarIndependent) * nSum / (agentUCount * timeStep));
		state[indexVarResponse] = (coopRatio >= targetCoopRatio) ? numLevelsVarResponse - 1
				: (int) (1.0 * (numLevelsVarResponse - 1) * coopRatio / targetCoopRatio);
//		// test
//		System.out.println("***setState(nSum="+nSum+",nLast="+nLast+",timeStep="+timeStep+")***");
//		System.out.println("calculated state: "+state[0]+","+state[1]);
//		System.out.println("*** end of setState ***");
//		// end test
	}

	private void runTD_SARSA() { // built in v3.0.0
		int timeStep, nSum, nLast, oldSignal;
		int[] oldState = new int[2];
		double coopRatio;
		// initialize Q
		initializeQ();
		// loop (episodes)
		for (int epi = 0; epi < episodesPerRun; epi++) {
			// initialize state
			state[indexVarIndependent] = 0;
			state[indexVarResponse] = 0;
			// set time to 0
			timeStep = 0;
			// reset registrar
			registrar.resetStats();
			episode.reset();
			episode.S[indexVarIndependent][timeStep] = state[indexVarIndependent];
			episode.S[indexVarResponse][timeStep] = state[indexVarResponse];
			// choose action (signal) based on state and policy derived from Q
			// update policy
			updatePolicy(state, epsilon);
			// update signal
			updateSignal();
			episode.A[timeStep] = signal;
			// oldState = state; // debugging: oops! this way future changes in state also change oldState!
			oldState[0] = state[0]; oldState[1] = state[1]; // this is the correct way!
			oldSignal = signal;
			// loop (steps)
			do {
//				// test
//				System.out.println("***runTD_SARSA()***");
//				System.out.println("old timeStep: "+timeStep);
//				System.out.println("oldState: "+state[0]+","+state[1]);
//				System.out.println("oldSignal: "+signal);
//				System.out.println("*");
//				// end test
				timeStep++;
//				// test
//				System.out.println("new timeStep: " + timeStep);
//				System.out.println("taking action: calling registrar to step()...");
//				// end test
				// take action (send signal), observe reward and new state
					// action
				registrar.step(); // signal is given to AgentU's and their response recorded.
				nLast = registrar.getNLast();
				nSum = registrar.getNSum();
				coopRatio = 1.0 * nLast / agentUCount;
					// reward
				episode.R[timeStep] = (coopRatio >= targetCoopRatio)? 0 : coopRatio - 1;
					// new state
				setState(nSum, nLast, timeStep);
				episode.S[indexVarIndependent][timeStep] = state[indexVarIndependent];
				episode.S[indexVarResponse][timeStep] = state[indexVarResponse];
				// choose new action (signal) based on state and policy derived from Q
				updatePolicy(state, epsilon);
				updateSignal();
				episode.A[timeStep] = signal;
				// update Q
//				// test
//				System.out.println("***still runTD_SARSA()***");
//				System.out.println("oldQ["+oldState[0]+"]["+oldState[1]+"]["+oldSignal+"]= "+Q[oldState[0]][oldState[1]][oldSignal]);
//				// end test
				Q[oldState[indexVarIndependent]][oldState[indexVarResponse]][oldSignal] +=
						alpha * (episode.R[timeStep] + // 20201225: change timestep-1 to timestep
								gamma * Q[state[indexVarIndependent]][state[indexVarResponse]][signal] -
								Q[oldState[indexVarIndependent]][oldState[indexVarResponse]][oldSignal]);
//				// test
//				System.out.println("newQ["+oldState[0]+"]["+oldState[1]+"]["+oldSignal+"]= "+Q[oldState[0]][oldState[1]][oldSignal]);
//				System.out.println("***");
//				// end test
				// update state and action (signal)
				// oldState = state; // debugging: oops! this way future changes in state also change oldState!
				oldState[0] = state[0]; oldState[1] = state[1]; // this is the correct way!
				oldSignal = signal;
				// until state is terminal (added in this implementation: or if stepsPerEpisode is reached)
			} while ((state[indexVarResponse] != (numLevelsVarResponse - 1)) & (timeStep < (stepsPerEpisode - 1)));
		}
	}

	private void runTD_QLearning() { // built in v3.1.0
		int timeStep, nSum, nLast, argmax;
		int[] oldState = new int[2];
		double coopRatio;
		// initialize Q
		initializeQ();
		// loop (episodes)
		for (int epi = 0; epi < episodesPerRun; epi++) {
			// initialize state
			state[indexVarIndependent] = 0;
			state[indexVarResponse] = 0;
			// set time to 0
			timeStep = 0;
			// reset registrar
			registrar.resetStats();
			episode.reset();
			episode.S[indexVarIndependent][timeStep] = state[indexVarIndependent];
			episode.S[indexVarResponse][timeStep] = state[indexVarResponse];
			oldState[0] = state[0]; oldState[1] = state[1]; // this is the correct way!
			// loop (steps)
			do {
//				// test
//				System.out.println("***runTD_QLearning()***");
//				System.out.println("old timeStep: "+timeStep);
//				System.out.println("oldState: "+state[0]+","+state[1]);
//				System.out.println("*");
//				// end test
				timeStep++; // 20201225 move to end of loop; adjust inside of loop accordingly // 20201227 moved here!
//				// choose action (signal) based on state and policy derived from Q
//				// update policy
				updatePolicy(state, epsilon);
//				// update signal // 20201227 note this is the action for PREVIOUS timeStep
				updateSignal();
				episode.A[timeStep-1] = signal; // 20201227 change timeStep to timeStep-1 because we have already upped timeStep before
//				// test
//				System.out.println("new timeStep: " + timeStep);
//				System.out.println("taking action: calling registrar to step()...");
//				// end test
				// take action (send signal), observe reward and new state
					// action
				registrar.step(); // signal is given to AgentU's and their response recorded.
				nLast = registrar.getNLast();
				nSum = registrar.getNSum();
				coopRatio = 1.0 * nLast / agentUCount;
					// reward
				episode.R[timeStep] = (coopRatio >= targetCoopRatio)? 0 : coopRatio - 1;
					// new state
				// here we are calculating state of the NEW TIMESTEP
				setState(nSum, nLast, timeStep);
				episode.S[indexVarIndependent][timeStep] = state[indexVarIndependent];
				episode.S[indexVarResponse][timeStep] = state[indexVarResponse];
//				// test
//				System.out.println("***still runTD_QLearning()***");
//				printQ();
//				System.out.println("state["+indexVarIndependent+"]="+state[indexVarIndependent]+
//						" , state["+indexVarResponse+"]="+state[indexVarResponse]);
//				// end test
				argmax = getArgmaxQ(state[indexVarIndependent],state[indexVarResponse]);
//				// test
//				System.out.println("argmax: "+argmax);
//				// end test
				Q[oldState[indexVarIndependent]][oldState[indexVarResponse]][signal] +=
						alpha * (episode.R[timeStep] + // 20201225 change timestep-1 to timestep
								gamma * Q[state[indexVarIndependent]][state[indexVarResponse]][argmax] -
								Q[oldState[indexVarIndependent]][oldState[indexVarResponse]][signal]);
//				// test
//				System.out.println("newQ["+oldState[0]+"]["+oldState[1]+"]["+signal+"]= "+Q[oldState[0]][oldState[1]][signal]);
//				System.out.println("***");
//				// end test
				// update state and action (signal)
				oldState[0] = state[0]; oldState[1] = state[1]; // this is the correct way!
				// until state is terminal (added in this implementation: or if stepsPerEpisode is reached)
			} while ((state[indexVarResponse] != (numLevelsVarResponse - 1)) & (timeStep < (stepsPerEpisode-1))); 
		}
	}
	
	private void runTD_expectedSARSA() { // 20201227 similar to Q-Learning, except instead of argmax uses expectation
		int timeStep, nSum, nLast;
		int[] oldState = new int[2];
		double coopRatio , expectedQ;
		// initialize Q
		initializeQ();
		// loop (episodes)
		for (int epi = 0; epi < episodesPerRun; epi++) {
			// initialize state
			state[indexVarIndependent] = 0;
			state[indexVarResponse] = 0;
			// set time to 0
			timeStep = 0;
			// reset registrar
			registrar.resetStats();
			episode.reset();
			episode.S[indexVarIndependent][timeStep] = state[indexVarIndependent];
			episode.S[indexVarResponse][timeStep] = state[indexVarResponse];
			oldState[0] = state[0]; oldState[1] = state[1]; // this is the correct way!
			// loop (steps)
			do {
//				// test
//				System.out.println("***runTD_expectedSARSA()***");
//				System.out.println("old timeStep: "+timeStep);
//				System.out.println("oldState: "+state[0]+","+state[1]);
//				System.out.println("*");
//				// end test
				timeStep++; // 20201225 move to end of loop; adjust inside of loop accordingly // 20201227 moved here!
//				// choose action (signal) based on state and policy derived from Q
//				// update policy
				updatePolicy(state, epsilon);
//				// update signal // 20201227 note this is the action for PREVIOUS timeStep
				updateSignal();
				episode.A[timeStep-1] = signal; // 20201227 change timeStep to timeStep-1 because we have already upped timeStep before
//				// test
//				System.out.println("new timeStep: " + timeStep);
//				System.out.println("taking action: calling registrar to step()...");
//				// end test
				// take action (send signal), observe reward and new state
					// action
				registrar.step(); // signal is given to AgentU's and their response recorded.
				nLast = registrar.getNLast();
				nSum = registrar.getNSum();
				coopRatio = 1.0 * nLast / agentUCount;
					// reward
				episode.R[timeStep] = (coopRatio >= targetCoopRatio)? 0 : coopRatio - 1;
					// new state
				// here we are calculating state of the NEW TIMESTEP
				setState(nSum, nLast, timeStep);
				episode.S[indexVarIndependent][timeStep] = state[indexVarIndependent];
				episode.S[indexVarResponse][timeStep] = state[indexVarResponse];
//				// test
//				System.out.println("***still runTD_expectedSARSA()***");
//				printQ();
//				System.out.println("state["+indexVarIndependent+"]="+state[indexVarIndependent]+
//						" , state["+indexVarResponse+"]="+state[indexVarResponse]);
//				// end test
				expectedQ = getExpectedQ(state[indexVarIndependent],state[indexVarResponse],epsilon); // epsilon is used: assuming greedy policies
//				// test
//				System.out.println("argmax: "+argmax);
//				// end test
				Q[oldState[indexVarIndependent]][oldState[indexVarResponse]][signal] +=
						alpha * (episode.R[timeStep] + // 20201225 change timestep-1 to timestep
								gamma * expectedQ - 
								Q[oldState[indexVarIndependent]][oldState[indexVarResponse]][signal]);
//				// test
//				System.out.println("newQ["+oldState[0]+"]["+oldState[1]+"]["+signal+"]= "+Q[oldState[0]][oldState[1]][signal]);
//				System.out.println("***");
//				// end test
				// update state and action (signal)
				oldState[0] = state[0]; oldState[1] = state[1]; // this is the correct way!
				// until state is terminal (added in this implementation: or if stepsPerEpisode is reached)
			} while ((state[indexVarResponse] != (numLevelsVarResponse - 1)) & (timeStep < (stepsPerEpisode-1))); 
		}
	}

	private void runTD_DoubleQLearning() { // built in v3.1.0
		int timeStep, nSum, nLast, argmax;
		int[] oldState = new int[2];
		double coopRatio;
		// initialize Q1 , Q2
		double[][][] Q1 = new double[numLevelsVarIndependent][numLevelsVarResponse][numActions];
		double[][][] Q2 = new double[numLevelsVarIndependent][numLevelsVarResponse][numActions];
		initializeQ(Q1);
		initializeQ(Q2);
		// loop (episodes)
		for (int epi = 0; epi < episodesPerRun; epi++) {
			// initialize state
			state[indexVarIndependent] = 0;
			state[indexVarResponse] = 0;
			// set time to 0
			timeStep = 0;
			// reset registrar
			registrar.resetStats();
			episode.reset();
			episode.S[indexVarIndependent][timeStep] = state[indexVarIndependent];
			episode.S[indexVarResponse][timeStep] = state[indexVarResponse];
			oldState[0] = state[0]; oldState[1] = state[1]; // this is the correct way!
			// loop (steps)
			do {
//				// test
//				System.out.println("***runTD_QLearning()***");
//				System.out.println("old timeStep: "+timeStep);
//				System.out.println("oldState: "+state[0]+","+state[1]);
//				System.out.println("*");
//				// end test
				timeStep++; // 20201225 move to end of loop; adjust inside of loop accordingly // 20201227 moved here!
//				// choose action (signal) based on state and policy derived from Q
//				// update policy
				updatePolicy(state, Q1, Q2, epsilon);
//				// update signal // 20201227 note this is the action for PREVIOUS timeStep
				updateSignal();
				episode.A[timeStep-1] = signal; // 20201227 change timeStep to timeStep-1 because we have already upped timeStep before
//				// test
//				System.out.println("new timeStep: " + timeStep);
//				System.out.println("taking action: calling registrar to step()...");
//				// end test
				// take action (send signal), observe reward and new state
					// action
				registrar.step(); // signal is given to AgentU's and their response recorded.
				nLast = registrar.getNLast();
				nSum = registrar.getNSum();
				coopRatio = 1.0 * nLast / agentUCount;
					// reward
				episode.R[timeStep] = (coopRatio >= targetCoopRatio)? 0 : coopRatio - 1;
					// new state
				// here we are calculating state of the NEW TIMESTEP
				setState(nSum, nLast, timeStep);
				episode.S[indexVarIndependent][timeStep] = state[indexVarIndependent];
				episode.S[indexVarResponse][timeStep] = state[indexVarResponse];
//				// test
//				System.out.println("***still runTD_QLearning()***");
//				printQ();
//				System.out.println("state["+indexVarIndependent+"]="+state[indexVarIndependent]+
//						" , state["+indexVarResponse+"]="+state[indexVarResponse]);
//				// end test
				// With 0.5 probability:
				if (Math.random() < 0.5) {
					// update Q1
					argmax = getArgmaxQ(state[indexVarIndependent],state[indexVarResponse],Q1);
					Q1[oldState[indexVarIndependent]][oldState[indexVarResponse]][signal] +=
							alpha * (episode.R[timeStep] + // 20201225 change timestep-1 to timestep
									gamma * Q2[state[indexVarIndependent]][state[indexVarResponse]][argmax] -
									Q1[oldState[indexVarIndependent]][oldState[indexVarResponse]][signal]);
				} else {
					// update Q2
					argmax = getArgmaxQ(state[indexVarIndependent],state[indexVarResponse],Q2);
					Q2[oldState[indexVarIndependent]][oldState[indexVarResponse]][signal] +=
							alpha * (episode.R[timeStep] + // 20201225 change timestep-1 to timestep
									gamma * Q1[state[indexVarIndependent]][state[indexVarResponse]][argmax] -
									Q2[oldState[indexVarIndependent]][oldState[indexVarResponse]][signal]);
				}
				
//				// test
//				System.out.println("argmax: "+argmax);
//				// end test
//				// test
//				System.out.println("newQ["+oldState[0]+"]["+oldState[1]+"]["+signal+"]= "+Q[oldState[0]][oldState[1]][signal]);
//				System.out.println("***");
//				// end test
				// update state and action (signal)
				oldState[0] = state[0]; oldState[1] = state[1]; // this is the correct way!
				// until state is terminal (added in this implementation: or if stepsPerEpisode is reached)
			} while ((state[indexVarResponse] != (numLevelsVarResponse - 1)) & (timeStep < (stepsPerEpisode-1))); 
		}
	}

	private void runTD_DoubleSARSA() { 
		int timeStep, nSum, nLast, oldSignal;
		int[] oldState = new int[2];
		double coopRatio;
		// initialize Q1 , Q2
		double[][][] Q1 = new double[numLevelsVarIndependent][numLevelsVarResponse][numActions];
		double[][][] Q2 = new double[numLevelsVarIndependent][numLevelsVarResponse][numActions];
		initializeQ(Q1);
		initializeQ(Q2);
		// loop (episodes)
		for (int epi = 0; epi < episodesPerRun; epi++) {
			// initialize state
			state[indexVarIndependent] = 0;
			state[indexVarResponse] = 0;
			// set time to 0
			timeStep = 0;
			// reset registrar
			registrar.resetStats();
			episode.reset();
			episode.S[indexVarIndependent][timeStep] = state[indexVarIndependent];
			episode.S[indexVarResponse][timeStep] = state[indexVarResponse];
			// choose action (signal) based on state and policy derived from Q
			// update policy
			updatePolicy(state, Q1, Q2, epsilon);
			// update signal
			updateSignal();
			episode.A[timeStep] = signal;
			// oldState = state; // debugging: oops! this way future changes in state also change oldState!
			oldState[0] = state[0]; oldState[1] = state[1]; // this is the correct way!
			oldSignal = signal;
			// loop (steps)
			do {
//				// test
//				System.out.println("***runTD_SARSA()***");
//				System.out.println("old timeStep: "+timeStep);
//				System.out.println("oldState: "+state[0]+","+state[1]);
//				System.out.println("oldSignal: "+signal);
//				System.out.println("*");
//				// end test
				timeStep++;
//				// test
//				System.out.println("new timeStep: " + timeStep);
//				System.out.println("taking action: calling registrar to step()...");
//				// end test
				// take action (send signal), observe reward and new state
					// action
				registrar.step(); // signal is given to AgentU's and their response recorded.
				nLast = registrar.getNLast();
				nSum = registrar.getNSum();
				coopRatio = 1.0 * nLast / agentUCount;
					// reward
				episode.R[timeStep] = (coopRatio >= targetCoopRatio)? 0 : coopRatio - 1;
					// new state
				setState(nSum, nLast, timeStep);
				episode.S[indexVarIndependent][timeStep] = state[indexVarIndependent];
				episode.S[indexVarResponse][timeStep] = state[indexVarResponse];
				// choose new action (signal) based on state and policy derived from Q
				updatePolicy(state, Q1, Q2, epsilon);
				updateSignal();
				episode.A[timeStep] = signal;
				// update Q
//				// test
//				System.out.println("***still runTD_DoubleSARSA()***");
//				System.out.println("oldQ["+oldState[0]+"]["+oldState[1]+"]["+oldSignal+"]= "+Q[oldState[0]][oldState[1]][oldSignal]);
//				// end test
				if (Math.random() < 0.5) {
					Q1[oldState[indexVarIndependent]][oldState[indexVarResponse]][oldSignal] +=
							alpha * (episode.R[timeStep] + // 20201225: change timestep-1 to timestep
									gamma * Q2[state[indexVarIndependent]][state[indexVarResponse]][signal] -
									Q1[oldState[indexVarIndependent]][oldState[indexVarResponse]][oldSignal]);
				} else {
					Q2[oldState[indexVarIndependent]][oldState[indexVarResponse]][oldSignal] +=
							alpha * (episode.R[timeStep] + // 20201225: change timestep-1 to timestep
									gamma * Q1[state[indexVarIndependent]][state[indexVarResponse]][signal] -
									Q2[oldState[indexVarIndependent]][oldState[indexVarResponse]][oldSignal]);					
				}
//				// test
//				System.out.println("newQ["+oldState[0]+"]["+oldState[1]+"]["+oldSignal+"]= "+Q[oldState[0]][oldState[1]][oldSignal]);
//				System.out.println("***");
//				// end test
				// update state and action (signal)
				// oldState = state; // debugging: oops! this way future changes in state also change oldState!
				oldState[0] = state[0]; oldState[1] = state[1]; // this is the correct way!
				oldSignal = signal;
				// until state is terminal (added in this implementation: or if stepsPerEpisode is reached)
			} while ((state[indexVarResponse] != (numLevelsVarResponse - 1)) & (timeStep < (stepsPerEpisode - 1)));
		}
	}

	private void runTD_DoubleExpectedSARSA() { 
		int timeStep, nSum, nLast;
		int[] oldState = new int[2];
		double coopRatio , expectedQ;
		// initialize Q1 , Q2
		double[][][] Q1 = new double[numLevelsVarIndependent][numLevelsVarResponse][numActions];
		double[][][] Q2 = new double[numLevelsVarIndependent][numLevelsVarResponse][numActions];
		initializeQ(Q1);
		initializeQ(Q2);
		// loop (episodes)
		for (int epi = 0; epi < episodesPerRun; epi++) {
			// initialize state
			state[indexVarIndependent] = 0;
			state[indexVarResponse] = 0;
			// set time to 0
			timeStep = 0;
			// reset registrar
			registrar.resetStats();
			episode.reset();
			episode.S[indexVarIndependent][timeStep] = state[indexVarIndependent];
			episode.S[indexVarResponse][timeStep] = state[indexVarResponse];
			oldState[0] = state[0]; oldState[1] = state[1]; // this is the correct way!
			// loop (steps)
			do {
//				// test
//				System.out.println("***runTD_expectedSARSA()***");
//				System.out.println("old timeStep: "+timeStep);
//				System.out.println("oldState: "+state[0]+","+state[1]);
//				System.out.println("*");
//				// end test
				timeStep++; // 20201225 move to end of loop; adjust inside of loop accordingly // 20201227 moved here!
//				// choose action (signal) based on state and policy derived from Q
//				// update policy
				updatePolicy(state, Q1, Q2, epsilon);
//				// update signal // 20201227 note this is the action for PREVIOUS timeStep
				updateSignal();
				episode.A[timeStep-1] = signal; // 20201227 change timeStep to timeStep-1 because we have already upped timeStep before
//				// test
//				System.out.println("new timeStep: " + timeStep);
//				System.out.println("taking action: calling registrar to step()...");
//				// end test
				// take action (send signal), observe reward and new state
					// action
				registrar.step(); // signal is given to AgentU's and their response recorded.
				nLast = registrar.getNLast();
				nSum = registrar.getNSum();
				coopRatio = 1.0 * nLast / agentUCount;
					// reward
				episode.R[timeStep] = (coopRatio >= targetCoopRatio)? 0 : coopRatio - 1;
					// new state
				// here we are calculating state of the NEW TIMESTEP
				setState(nSum, nLast, timeStep);
				episode.S[indexVarIndependent][timeStep] = state[indexVarIndependent];
				episode.S[indexVarResponse][timeStep] = state[indexVarResponse];
//				// test
//				System.out.println("***still runTD_expectedSARSA()***");
//				printQ();
//				System.out.println("state["+indexVarIndependent+"]="+state[indexVarIndependent]+
//						" , state["+indexVarResponse+"]="+state[indexVarResponse]);
//				// end test
				if (Math.random() < 0.5) {
					expectedQ = getExpectedQ(state[indexVarIndependent],state[indexVarResponse],Q2,epsilon); // epsilon is used: assuming greedy policies
					Q1[oldState[indexVarIndependent]][oldState[indexVarResponse]][signal] +=
							alpha * (episode.R[timeStep] + // 20201225 change timestep-1 to timestep
									gamma * expectedQ - 
									Q1[oldState[indexVarIndependent]][oldState[indexVarResponse]][signal]);
				} else {
					expectedQ = getExpectedQ(state[indexVarIndependent],state[indexVarResponse],Q1,epsilon); // epsilon is used: assuming greedy policies
					Q2[oldState[indexVarIndependent]][oldState[indexVarResponse]][signal] +=
							alpha * (episode.R[timeStep] + // 20201225 change timestep-1 to timestep
									gamma * expectedQ - 
									Q2[oldState[indexVarIndependent]][oldState[indexVarResponse]][signal]);					
				}
//				// test
//				System.out.println("newQ["+oldState[0]+"]["+oldState[1]+"]["+signal+"]= "+Q[oldState[0]][oldState[1]][signal]);
//				System.out.println("***");
//				// end test
				// update state and action (signal)
				oldState[0] = state[0]; oldState[1] = state[1]; // this is the correct way!
				// until state is terminal (added in this implementation: or if stepsPerEpisode is reached)
			} while ((state[indexVarResponse] != (numLevelsVarResponse - 1)) & (timeStep < (stepsPerEpisode-1))); 
		}
	}
	
	private void runMC_offPolicy() { // EveryVisit
		double[][] b = new double[numLevelsVarIndependent][numLevelsVarResponse]; // behavior policy
		double G , W;
		int S_ind_t, S_res_t, A_t; // auxiliary variables used as indices of episode vectors, for better readability
		int A_byPolicy;
		int[] tempState = new int[2];
		// initialize Q
		initializeQ();
		// initialize C(s,a)=0 for all (s,a)
		double[][][] C = new double[numLevelsVarIndependent][numLevelsVarResponse][numActions];
		for (int i = 0; i < numLevelsVarIndependent; i++) {
			for (int j = 0; j < numLevelsVarResponse; j++) {
				for (int k = 0; k < numActions; k++) {
					C[i][j][k] = 0;
				}
			}
		}
		// initialize target policy : pi(s) = argmax_a(Q(s,a)) with ties broken consistently
			// note we defined policy as the probability of choosing action1 rather than action0.
		for (int i = 0; i < numLevelsVarIndependent; i++) {
			for (int j = 0; j < numLevelsVarResponse; j++) {
				policy[i][j] = getArgmaxQ(i,j); // if argmax==1 then the prob of selecting action1 is 1. else, if argmax==0 then the prob of selecting action1 is 0.
			} // note: function getArgmaxQ() calls function breakTiesRandomly()
		}
		
		// loop for each episode
		for (int epi=0 ; epi < episodesPerRun ; epi++) {
			// b = any soft behavior policy
			for (int i = 0; i < numLevelsVarIndependent; i++) {
				for (int j = 0; j < numLevelsVarResponse; j++) {
					do {
						b[i][j] = Math.random(); // note:   0 <=   the value made by Math.random()   < 1
					} while (b[i][j]==0); // to make sure non-zero values are assigned.
				}
			}
//			// debugging
//			System.out.println("episode "+epi+ " printing behavior policy b:");
//			for (int i = 0; i < numLevelsVarIndependent; i++) {
//				for (int j = 0; j < numLevelsVarResponse; j++) {
//					 System.out.print(String.format("%.2f", b[i][j])+" ");
//				}
//				System.out.println();
//			}
//			//
			// generate episode following behavior policy 
			episode.reset();
			state[indexVarIndependent] = 0;
			state[indexVarResponse] = 0;
			generateEpisode(state[indexVarIndependent],state[indexVarResponse],b);
			// G = 0 // gain
			G = 0;
			// W = 1 // auxiliary variable for incremental implementation
			W = 1;
			// loop for each step t of episode from t=T-1 to t=0
			for (int timeStep=stepsPerEpisode-1 ; timeStep >= 0 ; timeStep--) { 
				S_ind_t = episode.S[indexVarIndependent][timeStep];
				// if (S_ind_t==-1) {S_ind_t = (int)(Math.random()*numLevelsVarIndependent);} // debugging final timeSteps effect
				S_res_t = episode.S[indexVarResponse][timeStep];
				// if (S_res_t==-1) {S_res_t = numLevelsVarResponse - 1;} // debugging final timeSteps effect
				A_t = episode.A[timeStep]; 
				// if (A_t==-1) {A_t=1;} // to debug the exceptional case of final timeStep of episode, in which I have set A_t=-1 for clarity in future analysis of results.
				if ((S_ind_t==-1) | (S_res_t==-1) | (A_t==-1)) {continue;} // debugging final timeSteps effect
				// G = gamma*G + R[t+1]
				G = gamma*G + episode.R[timeStep]; 
				// C(S[t],A[t]) += W
//				// debugging
//				System.out.println("episode "+epi+" , timeStep "+timeStep+" , S_ind_t="+S_ind_t+" , S_res_t="+S_res_t+" , A_t="+A_t);
//				//
				C[S_ind_t][S_res_t][A_t] += W;
				// Q(S[t],A[t]) += [W/C(S[t],A[t])]*{G-Q(S[t],A[t])}
				Q[S_ind_t][S_res_t][A_t] += (W/C[S_ind_t][S_res_t][A_t]) * (G-Q[S_ind_t][S_res_t][A_t]);
				// pi(S[t]) = argmax_a(Q(s[t],a))        with ties broken consistently
				//policy[S_ind_t][S_res_t] = (Math.random() < epsilon)? Math.random() : getArgmaxQ(S_ind_t , S_res_t);
				tempState[indexVarIndependent] = S_ind_t;
				tempState[indexVarResponse] = S_res_t;
				updatePolicy(tempState , epsilon);
				// if A[t]!=pi(S[t]) then exit inner loop (and proceed to next episode)
				A_byPolicy = (Math.random() < policy[S_ind_t][S_res_t])? 1 : 0;
				//if (A_t != policy[S_ind_t][S_res_t]) {break;}
				if (A_t != A_byPolicy) {break;}
				// W /= b(A[t]|S[t])
				W /= (A_t==1)? b[S_ind_t][S_res_t] : 1-b[S_ind_t][S_res_t];
			}
		}
		// the above procedure supposedly builds policy, but not episode based on policy. now we make an episode based on best learned policy
		episode.reset();
		generateEpisode(0,0,policy); // from state 0,0 and latest policy
	}

	private void runMC_onPolicy() { // EveryVisit
		//double[][] b = new double[numLevelsVarIndependent][numLevelsVarResponse]; // behavior policy
		double G , W;
		int S_ind_t, S_res_t, A_t; // auxiliary variables used as indices of episode vectors, for better readability
		int A_byPolicy;
		int[] tempState = new int[2];
		// initialize Q
		initializeQ();
		// initialize C(s,a)=0 for all (s,a)
		double[][][] C = new double[numLevelsVarIndependent][numLevelsVarResponse][numActions];
		for (int i = 0; i < numLevelsVarIndependent; i++) {
			for (int j = 0; j < numLevelsVarResponse; j++) {
				for (int k = 0; k < numActions; k++) {
					C[i][j][k] = 0;
				}
			}
		}
		// initialize target policy : pi(s) = argmax_a(Q(s,a)) with ties broken consistently
			// note we defined policy as the probability of choosing action1 rather than action0.
		for (int i = 0; i < numLevelsVarIndependent; i++) {
			for (int j = 0; j < numLevelsVarResponse; j++) {
				policy[i][j] = getArgmaxQ(i,j); // if argmax==1 then the prob of selecting action1 is 1. else, if argmax==0 then the prob of selecting action1 is 0.
			} // note: function getArgmaxQ() calls function breakTiesRandomly()
		}
		
		// loop for each episode
		for (int epi=0 ; epi < episodesPerRun ; epi++) {
//			// b = any soft behavior policy
//			for (int i = 0; i < numLevelsVarIndependent; i++) {
//				for (int j = 0; j < numLevelsVarResponse; j++) {
//					do {
//						b[i][j] = Math.random(); // note:   0 <=   the value made by Math.random()   < 1
//					} while (b[i][j]==0); // to make sure non-zero values are assigned.
//				}
//			}
//			// debugging
//			System.out.println("episode "+epi+ " printing behavior policy b:");
//			for (int i = 0; i < numLevelsVarIndependent; i++) {
//				for (int j = 0; j < numLevelsVarResponse; j++) {
//					 System.out.print(String.format("%.2f", b[i][j])+" ");
//				}
//				System.out.println();
//			}
//			//
			// generate episode following behavior policy 
			episode.reset();
			state[indexVarIndependent] = 0;
			state[indexVarResponse] = 0;
			//generateEpisode(state[indexVarIndependent],state[indexVarResponse],b);
			generateEpisode(state[indexVarIndependent],state[indexVarResponse],policy);
			// G = 0 // gain
			G = 0;
			// W = 1 // auxiliary variable for incremental implementation
			W = 1;
			// loop for each step t of episode from t=T-1 to t=0
			for (int timeStep=stepsPerEpisode-1 ; timeStep >= 0 ; timeStep--) { 
				S_ind_t = episode.S[indexVarIndependent][timeStep];
				// if (S_ind_t==-1) {S_ind_t = (int)(Math.random()*numLevelsVarIndependent);} // debugging final timeSteps effect
				S_res_t = episode.S[indexVarResponse][timeStep];
				// if (S_res_t==-1) {S_res_t = numLevelsVarResponse - 1;} // debugging final timeSteps effect
				A_t = episode.A[timeStep]; 
				// if (A_t==-1) {A_t=1;} // to debug the exceptional case of final timeStep of episode, in which I have set A_t=-1 for clarity in future analysis of results.
				if ((S_ind_t==-1) | (S_res_t==-1) | (A_t==-1)) {continue;} // debugging final timeSteps effect
				// G = gamma*G + R[t+1]
				G = gamma*G + episode.R[timeStep]; 
				// C(S[t],A[t]) += W
//				// debugging
//				System.out.println("episode "+epi+" , timeStep "+timeStep+" , S_ind_t="+S_ind_t+" , S_res_t="+S_res_t+" , A_t="+A_t);
//				//
				C[S_ind_t][S_res_t][A_t] += W;
				// Q(S[t],A[t]) += [W/C(S[t],A[t])]*{G-Q(S[t],A[t])}
				Q[S_ind_t][S_res_t][A_t] += (W / C[S_ind_t][S_res_t][A_t]) * (G - Q[S_ind_t][S_res_t][A_t]);
				// pi(S[t]) = argmax_a(Q(s[t],a))        with ties broken consistently
				//policy[S_ind_t][S_res_t] = (Math.random() < epsilon)? Math.random() : getArgmaxQ(S_ind_t , S_res_t);
				tempState[indexVarIndependent] = S_ind_t;
				tempState[indexVarResponse] = S_res_t;
				updatePolicy(tempState , epsilon);
				// if A[t]!=pi(S[t]) then exit inner loop (and proceed to next episode)
//				if (A_t != policy[S_ind_t][S_res_t]) {break;}
//				// if algorithm is "MonteCarlo_offPolicy" then W /= b(A[t]|S[t])
//				if (Objects.equals(algorithm, "MonteCarlo_offPolicy")) {
//					W /= (A_t==1)? b[S_ind_t][S_res_t] : 1-b[S_ind_t][S_res_t]; 
//				} // note b is the probability of choosing action1
			}
		}
		// the above procedure supposedly builds policy, but not episode based on policy. now we make an episode based on best learned policy
		episode.reset();
		generateEpisode(0,0,policy); // from state 0,0 and latest policy
	}
	
	private int breakTiesRandomly(double[] vec, double val) { // tested OK
		int n = vec.length;
		int[] items = new int[n];		
		int itemsCount = 0;
		for (int i=0 ; i<n ; i++) {
			if (vec[i]==val) {
				items[itemsCount] = i;
				itemsCount++;
			}
		}
		int randIndex = (int)(Math.random()*itemsCount);
		return(items[randIndex]);
	}
	
	private double getExpectedQ(int s0, int s1, double epsilon) {
		// finding maximum Q value for the given state
		double qmax = Q[s0][s1][0];
		for (int i=1 ; i < numActions ; i++) {
			if (Q[s0][s1][i] > qmax) {qmax = Q[s0][s1][i];}
		}
		// finding repeated qmax values if any
		int numGreedy = 0;
		for (int i=0 ; i < numActions ; i++) {
			if (Q[s0][s1][i]==qmax) {numGreedy++;}
		}
		// probabilities of nongreedy and greedy actions in greedy policy
		double probNonGreedy = epsilon / numActions;
		double probGreedy = (1-epsilon)/numGreedy + probNonGreedy;
		// calculating expectedQ
		double coeff;
		double expectedQ = 0;
		for (int i=0 ; i < numActions ; i++) {
			coeff = (Q[s0][s1][i]==qmax) ? probGreedy : probNonGreedy ;
			expectedQ += coeff * Q[s0][s1][i];
		}
		return expectedQ;
	}
	
	private double getExpectedQ(int s0, int s1, double[][][] Q, double epsilon) {
		// finding maximum Q value for the given state
		double qmax = Q[s0][s1][0];
		for (int i=1 ; i < numActions ; i++) {
			if (Q[s0][s1][i] > qmax) {qmax = Q[s0][s1][i];}
		}
		// finding repeated qmax values if any
		int numGreedy = 0;
		for (int i=0 ; i < numActions ; i++) {
			if (Q[s0][s1][i]==qmax) {numGreedy++;}
		}
		// probabilities of nongreedy and greedy actions in greedy policy
		double probNonGreedy = epsilon / numActions;
		double probGreedy = (1-epsilon)/numGreedy + probNonGreedy;
		// calculating expectedQ
		double coeff;
		double expectedQ = 0;
		for (int i=0 ; i < numActions ; i++) {
			coeff = (Q[s0][s1][i]==qmax) ? probGreedy : probNonGreedy ;
			expectedQ += coeff * Q[s0][s1][i];
		}
		return expectedQ;
	}

	private void updateSignal() { // built in v3.0.0 calculates signal based on policy & state
		signal = (Math.random() < policy[state[indexVarIndependent]][state[indexVarResponse]]) ? 1 : 0;
//		// test
//		System.out.println("***updateSignal()***");
//		System.out.println("signal: " + signal);
//		System.out.println("***");
//		// end test
	}
	
	private void updateSignal(double[][] policy) {
//		// debugging
//		System.out.println("debugging updateSignal(double[][] policy)");
//		System.out.print("state[indexVarIndependent]="+state[indexVarIndependent]);
//		System.out.println("    state[indexVarResponse]="+state[indexVarResponse]);
//		System.out.println("end of debugging updateSignal(double[][] policy)");
//		//
		signal = (Math.random() < policy[state[indexVarIndependent]][state[indexVarResponse]]) ? 1 : 0;
	}

	private void updatePolicy(int[] tempState) { // built in v3.0.0 calculates policy based on Q and state. probabilistic.
		int s0 = tempState[indexVarIndependent];
		int s1 = tempState[indexVarResponse];
		double qa0 = Q[s0][s1][0];
		double qa1 = Q[s0][s1][1];
		policy[s0][s1] = 0.5 + 0.5 * (qa1 - qa0) / (Math.abs(qa0) + Math.abs(qa1));
	}

	private void updatePolicy(int[] tempState, double epsilon) { // overloading. epsilon-soft
		double res;
		int s0 = tempState[indexVarIndependent];
		int s1 = tempState[indexVarResponse];
		int a_star = getArgmaxQ(s0,s1);
		res = (a_star==1)? 1 - epsilon + epsilon/numActions : epsilon/numActions;
//		if (Math.random() < epsilon) {
//			// exploration
//			res = Math.random();
//		} else {
//			// exploitation
//			res = (Q[s0][s1][0] < Q[s0][s1][1]) ? 1.0 : 0;
//		}
		policy[s0][s1] = res;
//		// test
//		System.out.println("***updatePolicy(state={"+s0+","+s1+"},epsilon="+epsilon+")***");
//		System.out.print("Q["+s0+"]["+s1+"][0]= " + Q[s0][s1][0] + " ; ");
//		System.out.println("Q["+s0+"]["+s1+"][1]= " + Q[s0][s1][1] + " ; ");
//		System.out.println("policy["+s0+"]["+s1+"]: " + policy[s0][s1]);
//		System.out.println("***");
//		// end test
	}

	private void updatePolicy(int[] tempState, double[][][] Q, double epsilon) { // overloading. epsilon-soft
		double res;
		int s0 = tempState[indexVarIndependent];
		int s1 = tempState[indexVarResponse];
		int a_star = getArgmaxQ(s0,s1,Q);
		res = (a_star==1)? 1 - epsilon + epsilon/numActions : epsilon/numActions;
		policy[s0][s1] = res;
	}
	
	private void updatePolicy(int[] tempState, double[][][] Q1, double[][][] Q2, double epsilon) { // overloading. epsilon-soft
		double res;
		int s0 = tempState[indexVarIndependent];
		int s1 = tempState[indexVarResponse];
		double[] vec = new double[((Q1[s0][s1].length) + (Q2[s0][s1].length))];
//		// debugging
//		System.out.println("***debugging updatePolicy(tempState,Q1,Q2,epsilon)***");
//		System.out.println("Q1[s0][s1].length="+Q1[s0][s1].length+" Q2[s0][s1].length="+Q2[s0][s1].length+" vec.length="+vec.length);
//		//
		for (int i=0; i<Q1[s0][s1].length; i++) {
//			// debugging
//			System.out.println("i="+i);
//			//
			vec[i] = Q1[s0][s1][i];}
		for (int j=0; j<Q2[s0][s1].length; j++) {vec[j+Q1[s0][s1].length] = Q2[s0][s1][j];}
		int a_star = getArgmax(vec) % numActions; // note: numActions=2. if argmax(vec) is 1 or 3 it means in Q1 or Q2 the argmax points to action1
		res = (a_star==1)? 1 - epsilon + epsilon/numActions : epsilon/numActions;
		policy[s0][s1] = res;
//		//
//		System.out.println("***end of debugging updatePolicy(tempState,Q1,Q2,epsilon)***");
//		//
	}
	
	private void initializeQ() { // built in v3.0.0 sets random values to (nonterminal S,A), 0 to (terminal S,A)
		for (int i = 0; i < numLevelsVarIndependent; i++) {
			for (int j = 0; j < numLevelsVarResponse; j++) {
				for (int k = 0; k < numActions; k++) {
					Q[i][j][k] = (j == (numLevelsVarResponse - 1)) ? 0 : - Math.random();
				}
			}
		}
//		// test
//		System.out.println("***initializeQ()***");
//		for (int i = 0; i < numLevelsVarIndependent; i++) {
//			for (int j = 0; j < numLevelsVarResponse; j++) {
//				for (int k = 0; k < numActions; k++) {
//					System.out.print("Q["+i+"]["+j+"]["+k+"]= "+Q[i][j][k]+" , ");
//				} System.out.println();
//			} 
//		}
//		System.out.println("***");
//		// end test
	}
	
	private void initializeQ(double[][][] Q) { 
		for (int i = 0; i < numLevelsVarIndependent; i++) {
			for (int j = 0; j < numLevelsVarResponse; j++) {
				for (int k = 0; k < numActions; k++) {
					Q[i][j][k] = (j == (numLevelsVarResponse - 1)) ? 0 : - Math.random();
				}
			}
		}
	}

	private void printQ() {
		System.out.println("*** printQ ***");
		for (int i = 0; i < numLevelsVarIndependent; i++) {
			for (int j = 0; j < numLevelsVarResponse; j++) {
				for (int k = 0; k < numActions; k++) {
					System.out.print("Q["+i+"]["+j+"]["+k+"]= "+String.format("%.2f", Q[i][j][k])+" , ");
				} System.out.print(" *** ");
			} System.out.println();
		} System.out.println("*** end of printQ ***");
	}

	private void initializeQ(double x) { // built in v3.0.0 overloading. sets fixed value x to (nonterminal S,A)
		for (int i = 0; i < numLevelsVarIndependent; i++) {
			for (int j = 0; j < numLevelsVarResponse; j++) {
				for (int k = 0; k < numActions; k++) {
					Q[i][j][k] = (j == (numLevelsVarResponse - 1)) ? 0 : x;
				}
			}
		}
	}
	
	// copied from RunMaker
	public static void writeArrayToFile(double[] arrayToWrite, String fileName) throws IOException {
		String outString = "";
		for (double x : arrayToWrite) {
			outString = outString + String.valueOf(x) + " ";
		}
		outString = outString.substring( 0 , outString.length() - 1 ); // to get rid of the last ","
		FileWriter fileWriter = new FileWriter(fileName, true);
		fileWriter.write(outString + "\n");
		fileWriter.close();		
	}
	
	// 
	public static void writeArrayToFile(int[] arrayToWrite, String fileName) throws IOException { // overloading: same function as above, only with int[] arrayToWrite
		String outString = "";
		for (int x : arrayToWrite) {
			outString = outString + String.valueOf(x) + " ";
		}
		outString = outString.substring( 0 , outString.length() - 1 ); // to get rid of the last ","
		FileWriter fileWriter = new FileWriter(fileName, true);
		fileWriter.write(outString + "\n");
		fileWriter.close();		
	}

}

class Episode {

	int[][] S; // v3.0.0
	int[] A;
	double[] R;
	int stepsPerEpisode;

	public Episode(int stepsPerEpisode) {
		this.stepsPerEpisode = stepsPerEpisode;
		this.S = new int[2][stepsPerEpisode]; // v3.0.0
		this.A = new int[stepsPerEpisode];
		this.R = new double[stepsPerEpisode]; // note: in Sutton's book indices of R are 1..T . in this code they are
												// 0..(T-1). note: initial values are 0.0 // v3.0.0 revoke note! back to Sutton's!
	}
	
	public void reset() { // built v3.0.0
		for (int i=0 ; i < stepsPerEpisode ; i++) {
			S[0][i] = -1; // to identify early end of episode
			S[1][i] = -1; // "
			A[i] = -1;    // "
			R[i] = 0; 
		}
		R[0] = -1;
	}
}