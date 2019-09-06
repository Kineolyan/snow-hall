package code4life;

import java.awt.Point;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Properties;
import java.util.Random;
import java.util.StringJoiner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class Referee extends MultiReferee {
	public static int LEAGUE_LEVEL = 3; // 0, 1 or 2. 3 is for silver+.

	public static final int EV_NEW_SAMPLE = 0;
	public static final int EV_TAKE_SAMPLE = 1;
	public static final int EV_STORE_SAMPLE = 2;
	public static final int EV_TAKE_RESOURCE = 3;
	public static final int EV_DIAGNOSE = 4;
	public static final int EV_PRODUCE = 5;
	public static final int EV_CLONE_INITAL_SAMPLE = 6;
	public static final int[] RESOURCE_PER_TYPE_BY_LEAGUE_LEVEL = {99, 99, 6, 5};
	public static final int[] SCIENCE_PROJECTS_BY_LEAGUE_LEVEL = {0, 0, 3, 3};
	public static final int[] INIT_DIAGNOSED_SAMPLES_BY_LEAGUE_LEVEL = {50, 0, 0, 0};
	public static final int MAX_STORAGE = 10;
	public static final int MAX_TRAY = 3;
	public static final int SAMPLE_RANK_COUNT = 3;
	public static final int SCIENCE_PROJECT_VALUE = LEAGUE_LEVEL < 3 ? 30 : 50;
	public static final int MAX_SCORE = 170;

	enum MoleculeType {
		A(0), B(1), C(2), D(3), E(4);

		int index;

		private MoleculeType(final int index) {
			this.index = index;
		}
	}

	enum Bound {
		TO_DIAGNOSIS, FROM_SAMPLES, FROM_DIAGNOSIS
	}

	enum Module {
		SAMPLES, DIAGNOSIS, MOLECULES, LABORATORY, START_POS
	}

	static class PlayerData {
		int[] storage, expertise;
		boolean dead, attemptConnection, moved;
		int eta, score, deadAt, index;
		String message, connectionData;
		List<Sample> tray;
		Module from, target;

		public PlayerData(final int index) {
			final int capacity = MoleculeType.values().length;
			this.from = Module.START_POS;
			this.target = Module.START_POS;
			this.eta = 0;
			this.storage = new int[capacity];
			this.expertise = new int[capacity];
			this.index = index;
			this.score = 0;
			this.tray = new ArrayList<>(3);
		}

		public void die(final int round) {
			if (!this.dead) {
				this.dead = true;
				this.deadAt = round;
				this.score = -1;
			}
		}

		public void reset() {
			this.message = null;
			this.attemptConnection = false;
			this.moved = false;
			this.connectionData = null;
		}

		public void setMessage(final String message) {
			this.message = message;
			if (message != null && message.length() > 19) {
				this.message = message.substring(0, 17) + "...";
			}
		}

		public boolean isMoving() {
			return this.eta > 0;
		}
	}

	static class Sample {
		public static int ENTITY_COUNT = 0;

		MoleculeType expertise;
		int life;
		int[] cost;
		int id, rank;
		private boolean discovered;
		PlayerData discoveredBy;

		public Sample(final int[] cost, final int life, final MoleculeType gain) {
			this.expertise = gain;
			this.life = life;
			this.cost = cost;
		}

		public void setDiscovered(final boolean discovered) {
			this.discovered = discovered;

		}

		public boolean isDiscovered() {
			return this.discovered;

		}

		public Sample clone() {
			return new Sample(this.cost, this.life, this.expertise);
		}

		public String getGainChar() {
			return (this.expertise == null) ? "0" : this.expertise.name();
		}
	}

	static class ScienceProject {
		int[] cost;
		int index;

		public ScienceProject(final int[] cost) {
			this.cost = cost;
		}
	}

	static class Diagnosis {
		PlayerData player;
		Sample sample;

		public Diagnosis(final PlayerData player, final Sample sample) {
			this.player = player;
			this.sample = sample;
		}
	}

	static class Translatable {
		String code;
		Object[] values;

		public Translatable(final String code, final Object... values) {
			this.code = code;
			this.values = values;
		}
	}

	static abstract class Transfer {
		PlayerData player;

		public Transfer(final PlayerData player) {
			this.player = player;
		}

		public abstract void apply(Referee refere);

		public abstract Translatable getSummary();
	}

	static class ProductionTransfer extends Transfer {
		Sample sample;

		public ProductionTransfer(final PlayerData player, final Sample sample) {
			super(player);
			this.sample = sample;
		}

		@Override
		public void apply(final Referee referee) {
			this.player.tray.remove(this.sample);

			for (int i = 0; i < MoleculeType.values().length; ++i) {
				final int toPay = Math.max(0, this.sample.cost[i] - this.player.expertise[i]);
				this.player.storage[i] -= toPay;
				final MoleculeType type = MoleculeType.values()[i];
				referee.molecules.put(type, referee.molecules.get(type) + toPay);
			}

			this.player.score += this.sample.life;
			if (this.sample.expertise != null) {
				this.player.expertise[this.sample.expertise.index]++;
			}
		}

		@Override
		public Translatable getSummary() {
			if (this.sample.expertise == null) {
				return new Translatable("productionNoGain", this.player.index, this.sample.id, this.sample.life);
			}
			return new Translatable("production", this.player.index, this.sample.id, this.sample.life, this.sample.expertise.name());
		}
	}

	static class SampleTransfer extends Transfer {
		Sample sample, clone;
		Bound bound;

		public SampleTransfer(final PlayerData player, final Sample sample, final Bound bound) {
			super(player);
			this.sample = sample;
			this.bound = bound;
		}

		@Override
		public void apply(final Referee referee) {
			if (this.bound.equals(Bound.TO_DIAGNOSIS)) {
				this.player.tray.remove(this.sample);
				if (referee.storedSamples.stream().noneMatch(stored -> stored.equals(this.sample))) {
					referee.storedSamples.add(this.sample);
				}

			} else if (this.bound.equals(Bound.FROM_SAMPLES)) {
				this.player.tray.add(this.sample);
			} else if (this.bound.equals(Bound.FROM_DIAGNOSIS)) {
				if (this.clone == null) {
					this.player.tray.add(this.sample);
					referee.storedSamples.remove(this.sample);
				} else {
					this.player.tray.add(this.clone);
				}
			}
		}

		@Override
		public Translatable getSummary() {
			if (this.bound.equals(Bound.TO_DIAGNOSIS)) {
				return new Translatable("upload", this.player.index, this.sample.id);
			} else if (this.bound.equals(Bound.FROM_SAMPLES)) {
				return new Translatable("newSample", this.player.index, this.sample.id);
			} else {
				return new Translatable("download", this.player.index, this.sample.id);
			}
		}

		public void setClone(final Sample clonedSample) {
			this.clone = clonedSample;
		}

	}

	static class ProjectCompletion {
		PlayerData player;
		ScienceProject project;

		public ProjectCompletion(final PlayerData player, final ScienceProject project) {
			this.player = player;
			this.project = project;
		}
	}

	static class ResourceTransfer extends Transfer {
		MoleculeType resourceType;

		public ResourceTransfer(final PlayerData player, final MoleculeType type) {
			super(player);
			this.resourceType = type;
		}

		@Override
		public void apply(final Referee referee) {
			this.player.storage[this.resourceType.index]++;
			referee.molecules.put(this.resourceType, referee.molecules.get(this.resourceType) - 1);
		}

		@Override
		public Translatable getSummary() {
			return new Translatable("takeMolecule", this.player.index, this.resourceType.name());
		}
	}

	static class ModulePair {
		HashSet<Module> set;

		public ModulePair(final Module a, final Module b) {
			this.set = new HashSet<>();
			this.set.add(a);
			this.set.add(b);
		}

		@Override
		public int hashCode() {
			return this.set.hashCode();
		}

		@Override
		public boolean equals(final Object obj) {
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (getClass() != obj.getClass())
				return false;
			final ModulePair other = (ModulePair) obj;
			return this.set.equals(other.set);
		}
	}

	static final Pattern PLAYER_MOVE_PATTERN = Pattern.compile("^GOTO\\s+(?<module>LABORATORY|DIAGNOSIS|MOLECULES|SAMPLES)(?:\\s+)?(?:\\s+(?<message>.+))?", Pattern.CASE_INSENSITIVE);
	static final Pattern PLAYER_WAIT_PATTERN = Pattern.compile("^WAIT(?:\\s+)?(?:\\s+(?<message>.+))?", Pattern.CASE_INSENSITIVE);
	static final Pattern PLAYER_USE_PATTERN = Pattern.compile("^CONNECT(?:\\s+(?<data>[ABCDE]|(?:-?\\d+)))?(?:\\s+)?(?:\\s+(?<message>.+))?$", Pattern.CASE_INSENSITIVE);
	static final String[] EXPECTED_BY_LEAGUE_LEVEL = {
			"GOTO LABORATORY|DIAGNOSIS|MOLECULES | CONNECT data",
			"GOTO LABORATORY|DIAGNOSIS|MOLECULES|SAMPLES | CONNECT data",
			"GOTO LABORATORY|DIAGNOSIS|MOLECULES|SAMPLES | CONNECT data",
			"GOTO LABORATORY|DIAGNOSIS|MOLECULES|SAMPLES | CONNECT data"
	};

	private List<PlayerData> players;
	private List<Transfer> transfers;
	private Map<Sample, SampleTransfer> cloudRequests;
	private List<Diagnosis> diagnoses;
	private List<ProjectCompletion> projectCompletions;
	private Random random;
	private List<LinkedList<Sample>> samplePool;
	List<Sample> storedSamples;
	List<ScienceProject> scienceProjects;
	private long seed;
	Map<MoleculeType, Integer> molecules;
	Map<ModulePair, Integer> distances;

	public Referee(final InputStream is, final PrintStream out, final PrintStream err) throws IOException {
		super(is, out, err);
	}

	@Override
	protected void initReferee(final int playerCount, final Properties prop) throws InvalidFormatException {
		this.seed = Long.valueOf(prop.getProperty("seed", String.valueOf(new Random(System.currentTimeMillis()).nextLong())));

		this.random = new Random(this.seed);

		// map
		initMap();

		// players
		this.players = new ArrayList<PlayerData>();
		for (int i = 0; i < playerCount; ++i) {
			this.players.add(i, new PlayerData(i));
		}

		// resources
		this.molecules = new HashMap<>();
		for (final MoleculeType type : MoleculeType.values()) {
			this.molecules.put(type, RESOURCE_PER_TYPE_BY_LEAGUE_LEVEL[LEAGUE_LEVEL]);
		}

		// samples
		initSamplePool();
		this.storedSamples = new LinkedList<>();

		// science
		initScienceProjects();

		// diagnosis
		initDiagnonisModule();

		this.transfers = new LinkedList<>();
		this.cloudRequests = new HashMap<>();
		this.diagnoses = new LinkedList<>();
		this.projectCompletions = new LinkedList<>();

	}

	private void initScienceProjects() {
		final LinkedList<ScienceProject> scienceProjectPool;
		scienceProjectPool = new LinkedList<>();
		scienceProjectPool.add(new ScienceProject(new int[]{3, 3, 0, 0, 3}));
		scienceProjectPool.add(new ScienceProject(new int[]{0, 3, 3, 3, 0}));
		scienceProjectPool.add(new ScienceProject(new int[]{3, 0, 0, 3, 3}));
		scienceProjectPool.add(new ScienceProject(new int[]{0, 0, 4, 4, 0}));
		scienceProjectPool.add(new ScienceProject(new int[]{0, 4, 4, 0, 0}));
		scienceProjectPool.add(new ScienceProject(new int[]{0, 0, 0, 4, 4}));
		scienceProjectPool.add(new ScienceProject(new int[]{4, 0, 0, 0, 4}));
		scienceProjectPool.add(new ScienceProject(new int[]{3, 3, 3, 0, 0}));
		scienceProjectPool.add(new ScienceProject(new int[]{0, 0, 3, 3, 3}));
		scienceProjectPool.add(new ScienceProject(new int[]{4, 4, 0, 0, 0}));
		Collections.shuffle(scienceProjectPool, this.random);

		this.scienceProjects = new ArrayList<>(SCIENCE_PROJECTS_BY_LEAGUE_LEVEL[LEAGUE_LEVEL]);
		for (int i = 0; i < SCIENCE_PROJECTS_BY_LEAGUE_LEVEL[LEAGUE_LEVEL]; ++i) {
			final ScienceProject project = scienceProjectPool.pop();
			project.index = i;
			this.scienceProjects.add(project);
		}
	}

	private void initSamplePool() {
		this.samplePool = new ArrayList<LinkedList<Sample>>(SAMPLE_RANK_COUNT);
		for (int rank = 0; rank < SAMPLE_RANK_COUNT; ++rank) {
			final LinkedList<Sample> cells = new LinkedList<Sample>();
			this.samplePool.add(cells);
		}
		this.samplePool.get(0).add(new Sample(new int[]{0, 3, 0, 0, 0}, 01, MoleculeType.A));
		this.samplePool.get(0).add(new Sample(new int[]{0, 0, 0, 2, 1}, 01, MoleculeType.A));
		this.samplePool.get(0).add(new Sample(new int[]{0, 1, 1, 1, 1}, 01, MoleculeType.A));
		this.samplePool.get(0).add(new Sample(new int[]{0, 2, 0, 0, 2}, 01, MoleculeType.A));
		this.samplePool.get(0).add(new Sample(new int[]{0, 0, 4, 0, 0}, 10, MoleculeType.A));
		this.samplePool.get(0).add(new Sample(new int[]{0, 1, 2, 1, 1}, 01, MoleculeType.A));
		this.samplePool.get(0).add(new Sample(new int[]{0, 2, 2, 0, 1}, 01, MoleculeType.A));
		this.samplePool.get(0).add(new Sample(new int[]{3, 1, 0, 0, 1}, 01, MoleculeType.A));
		this.samplePool.get(0).add(new Sample(new int[]{1, 0, 0, 0, 2}, 01, MoleculeType.B));
		this.samplePool.get(0).add(new Sample(new int[]{0, 0, 0, 0, 3}, 01, MoleculeType.B));
		this.samplePool.get(0).add(new Sample(new int[]{1, 0, 1, 1, 1}, 01, MoleculeType.B));
		this.samplePool.get(0).add(new Sample(new int[]{0, 0, 2, 0, 2}, 01, MoleculeType.B));
		this.samplePool.get(0).add(new Sample(new int[]{0, 0, 0, 4, 0}, 10, MoleculeType.B));
		this.samplePool.get(0).add(new Sample(new int[]{1, 0, 1, 2, 1}, 01, MoleculeType.B));
		this.samplePool.get(0).add(new Sample(new int[]{1, 0, 2, 2, 0}, 01, MoleculeType.B));
		this.samplePool.get(0).add(new Sample(new int[]{0, 1, 3, 1, 0}, 01, MoleculeType.B));
		this.samplePool.get(0).add(new Sample(new int[]{2, 1, 0, 0, 0}, 01, MoleculeType.C));
		this.samplePool.get(0).add(new Sample(new int[]{0, 0, 0, 3, 0}, 01, MoleculeType.C));
		this.samplePool.get(0).add(new Sample(new int[]{1, 1, 0, 1, 1}, 01, MoleculeType.C));
		this.samplePool.get(0).add(new Sample(new int[]{0, 2, 0, 2, 0}, 01, MoleculeType.C));
		this.samplePool.get(0).add(new Sample(new int[]{0, 0, 0, 0, 4}, 10, MoleculeType.C));
		this.samplePool.get(0).add(new Sample(new int[]{1, 1, 0, 1, 2}, 01, MoleculeType.C));
		this.samplePool.get(0).add(new Sample(new int[]{0, 1, 0, 2, 2}, 01, MoleculeType.C));
		this.samplePool.get(0).add(new Sample(new int[]{1, 3, 1, 0, 0}, 01, MoleculeType.C));
		this.samplePool.get(0).add(new Sample(new int[]{0, 2, 1, 0, 0}, 01, MoleculeType.D));
		this.samplePool.get(0).add(new Sample(new int[]{3, 0, 0, 0, 0}, 01, MoleculeType.D));
		this.samplePool.get(0).add(new Sample(new int[]{1, 1, 1, 0, 1}, 01, MoleculeType.D));
		this.samplePool.get(0).add(new Sample(new int[]{2, 0, 0, 2, 0}, 01, MoleculeType.D));
		this.samplePool.get(0).add(new Sample(new int[]{4, 0, 0, 0, 0}, 10, MoleculeType.D));
		this.samplePool.get(0).add(new Sample(new int[]{2, 1, 1, 0, 1}, 01, MoleculeType.D));
		this.samplePool.get(0).add(new Sample(new int[]{2, 0, 1, 0, 2}, 01, MoleculeType.D));
		this.samplePool.get(0).add(new Sample(new int[]{1, 0, 0, 1, 3}, 01, MoleculeType.D));
		this.samplePool.get(0).add(new Sample(new int[]{0, 0, 2, 1, 0}, 01, MoleculeType.E));
		this.samplePool.get(0).add(new Sample(new int[]{0, 0, 3, 0, 0}, 01, MoleculeType.E));
		this.samplePool.get(0).add(new Sample(new int[]{1, 1, 1, 1, 0}, 01, MoleculeType.E));
		this.samplePool.get(0).add(new Sample(new int[]{2, 0, 2, 0, 0}, 01, MoleculeType.E));
		this.samplePool.get(0).add(new Sample(new int[]{0, 4, 0, 0, 0}, 10, MoleculeType.E));
		this.samplePool.get(0).add(new Sample(new int[]{1, 2, 1, 1, 0}, 01, MoleculeType.E));
		this.samplePool.get(0).add(new Sample(new int[]{2, 2, 0, 1, 0}, 01, MoleculeType.E));
		this.samplePool.get(0).add(new Sample(new int[]{0, 0, 1, 3, 1}, 01, MoleculeType.E));
		this.samplePool.get(1).add(new Sample(new int[]{0, 0, 0, 5, 0}, 20, MoleculeType.A));
		this.samplePool.get(1).add(new Sample(new int[]{6, 0, 0, 0, 0}, 30, MoleculeType.A));
		this.samplePool.get(1).add(new Sample(new int[]{0, 0, 3, 2, 2}, 10, MoleculeType.A));
		this.samplePool.get(1).add(new Sample(new int[]{0, 0, 1, 4, 2}, 20, MoleculeType.A));
		this.samplePool.get(1).add(new Sample(new int[]{2, 3, 0, 3, 0}, 10, MoleculeType.A));
		this.samplePool.get(1).add(new Sample(new int[]{0, 0, 0, 5, 3}, 20, MoleculeType.A));
		this.samplePool.get(1).add(new Sample(new int[]{0, 5, 0, 0, 0}, 20, MoleculeType.B));
		this.samplePool.get(1).add(new Sample(new int[]{0, 6, 0, 0, 0}, 30, MoleculeType.B));
		this.samplePool.get(1).add(new Sample(new int[]{0, 2, 2, 3, 0}, 10, MoleculeType.B));
		this.samplePool.get(1).add(new Sample(new int[]{2, 0, 0, 1, 4}, 20, MoleculeType.B));
		this.samplePool.get(1).add(new Sample(new int[]{0, 2, 3, 0, 3}, 20, MoleculeType.B));
		this.samplePool.get(1).add(new Sample(new int[]{5, 3, 0, 0, 0}, 20, MoleculeType.B));
		this.samplePool.get(1).add(new Sample(new int[]{0, 0, 5, 0, 0}, 20, MoleculeType.C));
		this.samplePool.get(1).add(new Sample(new int[]{0, 0, 6, 0, 0}, 30, MoleculeType.C));
		this.samplePool.get(1).add(new Sample(new int[]{2, 3, 0, 0, 2}, 10, MoleculeType.C));
		this.samplePool.get(1).add(new Sample(new int[]{3, 0, 2, 3, 0}, 10, MoleculeType.C));
		this.samplePool.get(1).add(new Sample(new int[]{4, 2, 0, 0, 1}, 20, MoleculeType.C));
		this.samplePool.get(1).add(new Sample(new int[]{0, 5, 3, 0, 0}, 20, MoleculeType.C));
		this.samplePool.get(1).add(new Sample(new int[]{5, 0, 0, 0, 0}, 20, MoleculeType.D));
		this.samplePool.get(1).add(new Sample(new int[]{0, 0, 0, 6, 0}, 30, MoleculeType.D));
		this.samplePool.get(1).add(new Sample(new int[]{2, 0, 0, 2, 3}, 10, MoleculeType.D));
		this.samplePool.get(1).add(new Sample(new int[]{1, 4, 2, 0, 0}, 20, MoleculeType.D));
		this.samplePool.get(1).add(new Sample(new int[]{0, 3, 0, 2, 3}, 10, MoleculeType.D));
		this.samplePool.get(1).add(new Sample(new int[]{3, 0, 0, 0, 5}, 20, MoleculeType.D));
		this.samplePool.get(1).add(new Sample(new int[]{0, 0, 0, 0, 5}, 20, MoleculeType.E));
		this.samplePool.get(1).add(new Sample(new int[]{0, 0, 0, 0, 6}, 30, MoleculeType.E));
		this.samplePool.get(1).add(new Sample(new int[]{3, 2, 2, 0, 0}, 10, MoleculeType.E));
		this.samplePool.get(1).add(new Sample(new int[]{0, 1, 4, 2, 0}, 20, MoleculeType.E));
		this.samplePool.get(1).add(new Sample(new int[]{3, 0, 3, 0, 2}, 10, MoleculeType.E));
		this.samplePool.get(1).add(new Sample(new int[]{0, 0, 5, 3, 0}, 20, MoleculeType.E));
		this.samplePool.get(2).add(new Sample(new int[]{0, 0, 0, 0, 7}, 40, MoleculeType.A));
		this.samplePool.get(2).add(new Sample(new int[]{3, 0, 0, 0, 7}, 50, MoleculeType.A));
		this.samplePool.get(2).add(new Sample(new int[]{3, 0, 0, 3, 6}, 40, MoleculeType.A));
		this.samplePool.get(2).add(new Sample(new int[]{0, 3, 3, 5, 3}, 30, MoleculeType.A));
		this.samplePool.get(2).add(new Sample(new int[]{7, 0, 0, 0, 0}, 40, MoleculeType.B));
		this.samplePool.get(2).add(new Sample(new int[]{7, 3, 0, 0, 0}, 50, MoleculeType.B));
		this.samplePool.get(2).add(new Sample(new int[]{6, 3, 0, 0, 3}, 40, MoleculeType.B));
		this.samplePool.get(2).add(new Sample(new int[]{3, 0, 3, 3, 5}, 30, MoleculeType.B));
		this.samplePool.get(2).add(new Sample(new int[]{0, 7, 0, 0, 0}, 40, MoleculeType.C));
		this.samplePool.get(2).add(new Sample(new int[]{0, 7, 3, 0, 0}, 50, MoleculeType.C));
		this.samplePool.get(2).add(new Sample(new int[]{3, 6, 3, 0, 0}, 40, MoleculeType.C));
		this.samplePool.get(2).add(new Sample(new int[]{5, 3, 0, 3, 3}, 30, MoleculeType.C));
		this.samplePool.get(2).add(new Sample(new int[]{0, 0, 7, 0, 0}, 40, MoleculeType.D));
		this.samplePool.get(2).add(new Sample(new int[]{0, 0, 7, 3, 0}, 50, MoleculeType.D));
		this.samplePool.get(2).add(new Sample(new int[]{0, 3, 6, 3, 0}, 40, MoleculeType.D));
		this.samplePool.get(2).add(new Sample(new int[]{3, 5, 3, 0, 3}, 30, MoleculeType.D));
		this.samplePool.get(2).add(new Sample(new int[]{0, 0, 0, 7, 0}, 40, MoleculeType.E));
		this.samplePool.get(2).add(new Sample(new int[]{0, 0, 0, 7, 3}, 50, MoleculeType.E));
		this.samplePool.get(2).add(new Sample(new int[]{0, 0, 3, 6, 3}, 40, MoleculeType.E));
		this.samplePool.get(2).add(new Sample(new int[]{3, 3, 5, 3, 0}, 30, MoleculeType.E));

		for (int rank = 0; rank < SAMPLE_RANK_COUNT; ++rank) {
			Collections.shuffle(this.samplePool.get(rank), this.random);
		}

	}

	private void initDiagnonisModule() {
		for (int i = 0; i < INIT_DIAGNOSED_SAMPLES_BY_LEAGUE_LEVEL[LEAGUE_LEVEL]; i++) {
			final int rank = 0;
			final Sample sample = this.samplePool.get(rank).pop();
			this.samplePool.get(rank).add(sample.clone());

			sample.id = Sample.ENTITY_COUNT++;
			sample.rank = rank;
			sample.setDiscovered(true);
			if (LEAGUE_LEVEL <= 1) {
				sample.expertise = null;
			}
			this.storedSamples.add(sample);
		}
	}

	private void initMap() {
		this.distances = new HashMap<>();
		if (LEAGUE_LEVEL >= 2) {
			this.distances.put(new ModulePair(Module.START_POS, Module.SAMPLES), 2);
			this.distances.put(new ModulePair(Module.START_POS, Module.DIAGNOSIS), 2);
			this.distances.put(new ModulePair(Module.START_POS, Module.MOLECULES), 2);
			this.distances.put(new ModulePair(Module.START_POS, Module.LABORATORY), 2);
			this.distances.put(new ModulePair(Module.SAMPLES, Module.DIAGNOSIS), 3);
			this.distances.put(new ModulePair(Module.SAMPLES, Module.MOLECULES), 3);
			this.distances.put(new ModulePair(Module.SAMPLES, Module.LABORATORY), 3);
			this.distances.put(new ModulePair(Module.DIAGNOSIS, Module.MOLECULES), 3);
			this.distances.put(new ModulePair(Module.DIAGNOSIS, Module.LABORATORY), 4);
			this.distances.put(new ModulePair(Module.MOLECULES, Module.LABORATORY), 3);
		} else {
			this.distances.put(new ModulePair(Module.START_POS, Module.SAMPLES), 1);
			this.distances.put(new ModulePair(Module.START_POS, Module.DIAGNOSIS), 1);
			this.distances.put(new ModulePair(Module.START_POS, Module.MOLECULES), 1);
			this.distances.put(new ModulePair(Module.START_POS, Module.LABORATORY), 1);
			this.distances.put(new ModulePair(Module.SAMPLES, Module.DIAGNOSIS), 1);
			this.distances.put(new ModulePair(Module.SAMPLES, Module.MOLECULES), 1);
			this.distances.put(new ModulePair(Module.SAMPLES, Module.LABORATORY), 1);
			this.distances.put(new ModulePair(Module.DIAGNOSIS, Module.MOLECULES), 1);
			this.distances.put(new ModulePair(Module.DIAGNOSIS, Module.LABORATORY), 1);
			this.distances.put(new ModulePair(Module.MOLECULES, Module.LABORATORY), 1);
		}
	}

	@Override
	protected Properties getConfiguration() {
		final Properties prop = new Properties();
		prop.setProperty("seed", String.valueOf(this.seed));
		return prop;
	}

	@Override
	protected String[] getInitInputForPlayer(final int playerIdx) {
		final List<String> lines = new ArrayList<>();
		lines.add(String.valueOf(this.scienceProjects.size()));
		for (final ScienceProject project : this.scienceProjects) {
			lines.add(resourceArrayToString(project.cost));
		}

		return lines.toArray(new String[lines.size()]);
	}

	@Override
	protected void prepare(final int round) {
		this.transfers.clear();
		this.diagnoses.clear();
		this.cloudRequests.clear();
		this.projectCompletions.clear();
		for (final PlayerData player : this.players) {
			player.reset();
		}
	}

	@Override
	protected String[] getInputForPlayer(final int round, final int playerIdx) {
		final List<String> lines = new ArrayList<>();
		final List<String> sampleLines = new ArrayList<>();

		final Stream<PlayerData> a = this.players.stream().filter(p -> (p.index == playerIdx));
		final Stream<PlayerData> b = this.players.stream().filter(p -> (p.index != playerIdx));
		final List<PlayerData> reordered = Stream.concat(a, b).collect(Collectors.toList());
		reordered.stream().forEachOrdered(player -> {
			final StringJoiner sj = new StringJoiner(" ");
			sj.add(player.target.name());
			sj.add(String.valueOf(player.eta));
			sj.add(String.valueOf(player.score));
			sj.add(resourceArrayToString(player.storage));
			sj.add(resourceArrayToString(player.expertise));

			for (final Sample sample : player.tray) {
				final int carrier = player.index == playerIdx ? 0 : 1;
				if (sample.isDiscovered()) {
					sampleLines.add(join(sample.id, carrier, sample.rank + 1, sample.getGainChar(), sample.life, resourceArrayToString(sample.cost)));
				} else {
					sampleLines.add(join(sample.id, carrier, sample.rank + 1, "0 -1 -1 -1 -1 -1 -1"));
				}
			}
			lines.add(sj.toString());
		});

		for (final Sample sample : this.storedSamples) {
			sampleLines.add(join(sample.id, -1, sample.rank + 1, sample.getGainChar(), sample.life, resourceArrayToString(sample.cost)));
		}

		lines.add(Arrays.stream(MoleculeType.values()).map(type -> String.valueOf(Math.max(0, this.molecules.get(type)))).collect(Collectors.joining(" ")));
		lines.add(String.valueOf(sampleLines.size()));
		lines.addAll(sampleLines);

		return lines.toArray(new String[lines.size()]);

	}

	@Override
	protected int getExpectedOutputLineCountForPlayer(final int playerIdx) {
		return 1;
	}

	@Override
	protected void handlePlayerOutput(final int frame, final int round, final int playerIdx, final String[] outputs) throws WinException, LostException, InvalidInputException {
		final String line = outputs[0];
		final PlayerData player = this.players.get(playerIdx);

		try {
			if (player.isMoving()) {
				player.setMessage(line);
				return;
			}

			Matcher match = PLAYER_MOVE_PATTERN.matcher(line);
			if (match.matches()) {
				// Movement
				final String module = match.group("module");

				final Module target = Module.valueOf(module.toUpperCase());
				if (target == Module.SAMPLES && LEAGUE_LEVEL == 0) {
					throw new InvalidInputException(EXPECTED_BY_LEAGUE_LEVEL[LEAGUE_LEVEL], line);
				}

				if (player.target != target) {
					player.from = player.target;
					player.target = target;
					player.eta = this.distances.get(new ModulePair(player.target, player.from));
				}

				// Message
				matchMessage(player, match);
				return;
			}

			match = PLAYER_USE_PATTERN.matcher(line);
			if (match.matches()) {
				// Connect to machine
				final String data = match.group("data");
				player.attemptConnection = true;
				player.connectionData = data;

				connectToMachine(player, data);

				// Message
				matchMessage(player, match);
				return;
			}

			match = PLAYER_WAIT_PATTERN.matcher(line);
			if (match.matches()) {
				// Message
				matchMessage(player, match);
				return;
			}

			throw new InvalidInputException(EXPECTED_BY_LEAGUE_LEVEL[LEAGUE_LEVEL], line);

		} catch (final LostException | InvalidInputException e) {
			player.die(round);
			throw e;
		} catch (final Exception e) {
			player.die(round);
			throw new InvalidInputException(EXPECTED_BY_LEAGUE_LEVEL[LEAGUE_LEVEL], line);
		}
	}

	private void connectToMachine(final PlayerData player, final String data) throws LostException {
		try {
			switch (player.target) {
			case SAMPLES:
				try {
					requestSample(player, Integer.valueOf(data));
				} catch (final NumberFormatException e) {
					throw new LostException("badSampleRank", data);
				}
				break;
			case MOLECULES:
				final MoleculeType molecule;
				try {
					molecule = MoleculeType.valueOf(data.toUpperCase());
				} catch (final Exception e) {
					throw new LostException("unknownMoleculeType", data != null ? data : "");
				}
				requestMolecule(player, molecule);
				break;
			case DIAGNOSIS:
				requestDiagnosis(player, Integer.valueOf(data));
				break;
			case LABORATORY:
				requestProduction(player, Integer.valueOf(data));
				break;
			case START_POS:
				throw new LostException("connectToNothing");
			default:
				break;
			}
		} catch (final LostException le) {
			le.setTooltipCode("InvalidConnect");
			throw le;
		}
	}

	private void requestProduction(final PlayerData player, final Integer data) throws LostException {
		if (data == null) {
			throw new LostException("nullIsInvalidSample");
		}

		final Optional<Sample> target = player.tray.stream().filter(sample -> data.equals(sample.id)).findFirst();
		if (target.isPresent()) {
			final Sample sample = target.get();
			if (canAfford(player, sample.cost)) {
				this.transfers.add(new ProductionTransfer(player, sample));
				return;
			}
			throw new LostException("cannotAffordSample", data);
		}
		throw new LostException("sampleNotInTray", data);
	}

	private boolean canAfford(final PlayerData player, final int[] cost) {
		for (int i = 0; i < MoleculeType.values().length; ++i) {
			if (player.expertise[i] + player.storage[i] < cost[i]) {
				return false;
			}
		}
		return true;
	}

	private void requestDiagnosis(final PlayerData player, final Integer data) throws LostException {
		if (data == null) {
			throw new LostException("nullIsInvalidSample");
		}

		Optional<Sample> target = player.tray.stream().filter(sample -> data.equals(sample.id)).findFirst();

		if (target.isPresent()) {
			final Sample sample = target.get();
			if (sample.isDiscovered()) {
				this.transfers.add(new SampleTransfer(player, sample, Bound.TO_DIAGNOSIS));
				return;
			} else if (!sample.isDiscovered()) {
				// Diagnose
				this.diagnoses.add(new Diagnosis(player, sample));
				sample.setDiscovered(true);
				sample.discoveredBy = player;
				return;
			}
		} else {
			target = this.storedSamples.stream().filter(sample -> data.equals(sample.id)).findFirst();
			if (!target.isPresent()) {
				throw new LostException("sampleNotFound", data);
			}
			if (player.tray.size() >= MAX_TRAY) {
				throw new LostException("trayIsFull");
			}
			final Sample sample = target.get();
			final SampleTransfer transfer = new SampleTransfer(player, sample, Bound.FROM_DIAGNOSIS);

			if (this.cloudRequests.get(sample) == null || transfer.player == sample.discoveredBy) {
				this.cloudRequests.put(sample, transfer);
			} else if (LEAGUE_LEVEL == 0) {
				final Sample clonedSample = sample.clone();
				clonedSample.id = Sample.ENTITY_COUNT++;
				clonedSample.setDiscovered(true);
				transfer.setClone(clonedSample);
				this.cloudRequests.put(clonedSample, transfer);
			}
		}

	}

	private void requestMolecule(final PlayerData player, final MoleculeType type) throws LostException {
		if (this.molecules.get(type) <= 0) {
			throw new LostException("notEnoughMolecules", type.name());
		}
		if (Arrays.stream(player.storage).sum() >= MAX_STORAGE) {
			throw new LostException("storageIsFull");
		}
		this.transfers.add(new ResourceTransfer(player, type));
	}

	private void requestSample(final PlayerData player, final int rank) throws LostException {
		if (player.tray.size() >= MAX_TRAY) {
			throw new LostException("trayIsFull");
		}

		if (rank < 1 || rank > 3) {
			// throw new LostException("badSampleRank", String.valueOf(rank));
		}

		final Sample sample = this.samplePool.get(rank - 1).pop();
		// Just recycle it right back in there.
		this.samplePool.get(rank - 1).add(sample.clone());

		sample.id = Sample.ENTITY_COUNT++;
		sample.rank = rank - 1;
		sample.setDiscovered(false);

		if (LEAGUE_LEVEL <= 1) {
			sample.expertise = null;
		}

		this.transfers.add(new SampleTransfer(player, sample, Bound.FROM_SAMPLES));

	}

	private void matchMessage(final PlayerData player, final Matcher match) {
		player.setMessage(match.group("message"));
	}

	public double distance(final Point a, final Point b) {
		return Math.sqrt(Math.pow(b.x - a.x, 2) + Math.pow(b.y - a.y, 2));
	}

	@Override
	protected void updateGame(final int round) throws GameOverException {
		// Move players
		for (final PlayerData player : this.players) {
			if (player.eta != 0) {
				player.eta--;
				player.moved = true;
			}
		}

		// Perform transfers
		for (final SampleTransfer transfer : this.cloudRequests.values()) {
			this.transfers.add(transfer);
		}
		for (final Transfer transfer : this.transfers) {
			transfer.apply(this);
		}

		// Check for science projects
		final List<Runnable> removes = new LinkedList<>();
		for (final PlayerData player : this.players) {
			for (final ScienceProject project : this.scienceProjects) {
				if (completedProject(player, project)) {
					removes.add(() -> {
						this.scienceProjects.remove(project);
					});
					player.score += SCIENCE_PROJECT_VALUE;
					this.projectCompletions.add(new ProjectCompletion(player, project));
					addToolTip(player.index, translate("ProjectTooltip", player.index));
				}
			}

		}
		for (final Runnable r : removes) {
			r.run();
		}

	}

	private boolean completedProject(final PlayerData player, final ScienceProject project) {
		for (int i = 0; i < project.cost.length; ++i) {
			if (player.expertise[i] < project.cost[i]) {
				return false;
			}
		}
		return true;
	}

	@Override
	protected void populateMessages(final Properties p) {
		p.put("notEnoughMolecules", "Invalid CONNECT: there are no %s type molecules left");
		p.put("trayIsFull", "Invalid CONNECT: your robot may not carry data for more than " + MAX_TRAY + " samples");
		p.put("storageIsFull", "Invalid CONNECT: your robot may not carry more than " + MAX_STORAGE + " molecules");
		p.put("nullIsInvalidSample", "Invalid CONNECT: you must specify a Sample ID to connect to this module");
		p.put("sampleNotFound", "Invalid CONNECT: the sample %d is not available");
		p.put("badSampleRank", "Invalid CONNECT: there is no sample with rank %s");
		p.put("sampleNotInTray", "Invalid CONNECT: you are not carrying sample %d");
		p.put("unknownMoleculeType", "Invalid CONNECT: invalid molecule %s");
		p.put("cannotAffordSample", "Invalid CONNECT: you do not have enough molecules/expertise to launch research on sample %d");
		p.put("connectToNothing", "Invalid CONNECT: you must go to a module before using the connect command");
		p.put("InvalidConnect", "Invalid CONNECT");
		p.put("ProjectTooltip", "$0 completes a science project!");
		p.put("production", "$%d researched medicine for sample %d, scored %d health points and gained expertise in molecule %s");
		p.put("productionNoGain", "$%d researched medicine for sample %d, scored %d health points");
		p.put("upload", "$%d stores sample %d on the cloud.");
		p.put("newSample", "$%d receives sample %d.");
		p.put("download", "$%d downloads sample %d from the cloud.");
		p.put("takeMolecule", "$%d receives a %s molecule.");
		p.put("etaSAMPLES", "$%d will arrive at the samples module in %d turns");
		p.put("etaDIAGNOSIS", "$%d will arrive at the diagnosis module in %d turns");
		p.put("etaMOLECULES", "$%d will arrive at the molecules module in %d turns");
		p.put("etaLABORATORY", "$%d will arrive at the laboratory module in %d turns");
		p.put("etaSAMPLESsingular", "$%d will arrive at the samples module in %d turn");
		p.put("etaDIAGNOSISsingular", "$%d will arrive at the diagnosis module in %d turn");
		p.put("etaMOLECULESsingular", "$%d will arrive at the molecules module in %d turn");
		p.put("etaLABORATORYsingular", "$%d will arrive at the laboratory module in %d turn");
		p.put("diagnosis", "$%d has diagnosed sample %d");
		p.put("projectCompletion", "$%d has completed the science project %d and scores " + SCIENCE_PROJECT_VALUE + " health points.");

	}

	@Override
	protected String[] getInitDataForView() {
		final List<String> lines = new ArrayList<>();
		lines.add(SCIENCE_PROJECT_VALUE + " " + LEAGUE_LEVEL);

		lines.add(String.valueOf(this.scienceProjects.size()));
		for (final ScienceProject project : this.scienceProjects) {
			lines.add(resourceArrayToString(project.cost));
		}

		lines.add(String.valueOf(this.storedSamples.size()));
		for (final Sample sample : this.storedSamples) {
			lines.add(join(sample.id, resourceArrayToString(sample.cost), sample.rank, sample.life, sample.expertise));
		}

		lines.add(0, String.valueOf(lines.size() + 1));
		return lines.toArray(new String[lines.size()]);
	}

	private String resourceArrayToString(final int[] array) {
		return Arrays.stream(array).mapToObj(Integer::toString).collect(Collectors.joining(" "));
	}

	protected String[] getFrameDataForView(final int round, final int frame, final boolean keyFrame) {
		final List<String> lines = new ArrayList<>();

		// Players
		for (final PlayerData player : this.players) {
			final Integer total = this.distances.get(new ModulePair(player.target, player.from));

			final StringJoiner joiner = new StringJoiner(" ");
			joiner.add(player.target.name());
			joiner.add(player.from.name());
			joiner.add(String.valueOf(player.eta));
			joiner.add(player.moved ? "1" : "0");
			joiner.add((total == null) ? "0" : String.valueOf(total));
			joiner.add(String.valueOf(resourceArrayToString(player.storage)));
			joiner.add(String.valueOf(resourceArrayToString(player.expertise)));
			joiner.add(String.valueOf(player.score));
			joiner.add(player.dead ? "1" : "0");
			joiner.add(";" + (player.message == null ? "" : player.message));
			lines.add(joiner.toString());
		}

		// Resources
		lines.add(Arrays.stream(MoleculeType.values()).map(type -> String.valueOf(this.molecules.get(type))).collect(Collectors.joining(" ")));

		// Events
		final List<String> eventLines = new LinkedList<>();
		for (final Transfer transfer : this.transfers) {
			if (transfer instanceof SampleTransfer) {
				final SampleTransfer st = (SampleTransfer) transfer;
				if (st.bound.equals(Bound.FROM_SAMPLES)) {
					eventLines.add(join(EV_NEW_SAMPLE, st.sample.id, resourceArrayToString(st.sample.cost), st.sample.rank, st.sample.life, st.sample.expertise, st.player.index));
				} else if (st.bound.equals(Bound.FROM_DIAGNOSIS)) {
					if (st.clone != null) {
						eventLines.add(join(EV_CLONE_INITAL_SAMPLE, st.clone.id, resourceArrayToString(st.clone.cost), st.clone.rank, st.clone.life, st.clone.expertise));
						eventLines.add(join(EV_TAKE_SAMPLE, st.clone.id, st.player.index));
					} else {
						eventLines.add(join(EV_TAKE_SAMPLE, st.sample.id, st.player.index));
					}
				} else {
					eventLines.add(join(EV_STORE_SAMPLE, st.sample.id, st.player.index));
				}
			} else if (transfer instanceof ResourceTransfer) {
				final ResourceTransfer rt = (ResourceTransfer) transfer;
				eventLines.add(join(EV_TAKE_RESOURCE, rt.resourceType, rt.player.index));
			} else {
				final ProductionTransfer pt = (ProductionTransfer) transfer;
				eventLines.add(join(EV_PRODUCE, pt.sample.id));
			}
		}
		for (final Diagnosis diagnosis : this.diagnoses) {
			eventLines.add(join(EV_DIAGNOSE, diagnosis.sample.id));
		}
		lines.add(String.valueOf(eventLines.size()));
		lines.addAll(eventLines);

		return lines.toArray(new String[lines.size()]);
	}

	@SafeVarargs
	static final <T> String join(final T... v) {
		return Stream.of(v).map(String::valueOf).collect(Collectors.joining(" "));
	}

	@Override
	protected String getGameName() {
		return "Roche";
	}

	@Override
	protected String getHeadlineAtGameStartForConsole() {
		return null;
	}

	@Override
	protected int getMinimumPlayerCount() {
		return 2;
	}

	@Override
	protected boolean showTooltips() {
		return true;
	}

	@Override
	protected String[] getPlayerActions(final int playerIdx, final int round) {
		return new String[0];
	}

	@Override
	protected boolean isPlayerDead(final int playerIdx) {
		return this.players.get(playerIdx).dead;
	}

	@Override
	protected String getDeathReason(final int playerIdx) {
		return "$" + playerIdx + ": Eliminated!";
	}

	@Override
	protected int getMillisTimeForRound() {
		return 50;
	}

	@Override
	protected int getScore(final int playerIdx) {
		final PlayerData player = this.players.get(playerIdx);
		return player.score;
	}

	@Override
	protected String[] getGameSummary(final int round) {
		final List<String> lines = new ArrayList<>();
		for (int i = 0; i < this.players.size(); ++i) {
			lines.addAll(getPlayerSummary(i, round));
		}
		return lines.toArray(new String[lines.size()]);
	}

	protected List<String> getPlayerSummary(final int playerIdx, final int round) {
		final List<String> lines = new ArrayList<>();
		final PlayerData player = this.players.get(playerIdx);

		for (final Transfer t : this.transfers) {
			if (t.player == player) {
				final Translatable summary = t.getSummary();
				lines.add(translate(summary.code, summary.values));
			}
		}

		for (final Diagnosis d : this.diagnoses) {
			if (d.player == player) {
				lines.add(translate("diagnosis", playerIdx, d.sample.id));
			}
		}

		if (player.isMoving()) {
			lines.add(translate("eta" + player.target + (player.eta == 1 ? "singular" : ""), playerIdx, player.eta));
		}

		for (final ProjectCompletion projectCompletion : this.projectCompletions) {
			if (projectCompletion.player == player) {
				lines.add(translate("projectCompletion", playerIdx, projectCompletion.project.index));
			}
		}

		if (player.dead) {
			if (player.deadAt == round) {
				lines.add(getDeathReason(playerIdx));
			}
		}
		return lines;
	}

	@Override
	protected void setPlayerTimeout(final int frame, final int round, final int playerIdx) {
		final PlayerData player = this.players.get(playerIdx);
		player.die(round);
	}

	@Override
	protected int getMaxRoundCount(final int playerCount) {
		return 200;
	}

	@Override
	protected boolean gameOver() {
		if (LEAGUE_LEVEL >= 3) {
			return super.gameOver();
		} else {
			return super.gameOver() || this.players.stream().anyMatch(p -> p.score >= MAX_SCORE);
		}
	}

	public static void main(final String... args) throws IOException {
		new Referee(System.in, System.out, System.err).start();
	}
}
