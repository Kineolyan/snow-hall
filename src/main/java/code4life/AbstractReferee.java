package code4life;

import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Arrays;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Properties;
import java.util.Scanner;
import java.util.Set;
import java.util.regex.Pattern;

abstract class AbstractReferee {
	private static final Pattern HEADER_PATTERN = Pattern.compile("\\[\\[(?<cmd>.+)\\] ?(?<lineCount>[0-9]+)\\]");
	private static final String LOST_PARSING_REASON_CODE = "INPUT";
	private static final String LOST_PARSING_REASON = "Failure: invalid input";

	protected static class PlayerStatus {
		private int id;
		private int score;
		private boolean lost, win;
		private String info;
		private String reasonCode;
		private String[] nextInput;

		public PlayerStatus(final int id) {
			this.id = id;
			this.lost = false;
			this.info = null;
		}

		public int getScore() {
			return this.score;
		}

		public boolean isLost() {
			return this.lost;
		}

		public String getInfo() {
			return this.info;
		}

		public int getId() {
			return this.id;
		}

		public String getReasonCode() {
			return this.reasonCode;
		}

		public String[] getNextInput() {
			return this.nextInput;
		}
	}

	private Properties messages = new Properties();

	@SuppressWarnings("serial")
	final class InvalidFormatException extends Exception {
		public InvalidFormatException(final String message) {
			super(message);
		}
	}

	@SuppressWarnings("serial")
	abstract class GameException extends Exception {
		private String reasonCode, tooltipCode;
		private Object[] values;

		public GameException(final String reasonCode, final Object... values) {
			this.reasonCode = reasonCode;
			this.values = values;
		}

		public void setTooltipCode(final String tooltipCode) {
			this.tooltipCode = tooltipCode;
		}

		public String getReason() {
			if (this.reasonCode != null) {
				return translate(this.reasonCode, this.values);
			} else {
				return null;
			}
		}

		public String getReasonCode() {
			return this.reasonCode;
		}

		public String getTooltipCode() {
			if (this.tooltipCode != null) {
				return this.tooltipCode;
			}
			return getReasonCode();
		}
	}

	@SuppressWarnings("serial")
	class LostException extends GameException {
		public LostException(final String reasonCode, final Object... values) {
			super(reasonCode, values);
		}
	}

	@SuppressWarnings("serial")
	class WinException extends GameException {
		public WinException(final String reasonCode, final Object... values) {
			super(reasonCode, values);
		}
	}

	@SuppressWarnings("serial")
	class InvalidInputException extends GameException {
		public InvalidInputException(final String expected, final String found) {
			super("InvalidInput", expected, found);
		}
	}

	@SuppressWarnings("serial")
	class GameOverException extends GameException {
		public GameOverException(final String reasonCode, final Object... values) {
			super(reasonCode, values);
		}
	}

	@SuppressWarnings("serial")
	class GameErrorException extends Exception {
		public GameErrorException(final Throwable cause) {
			super(cause);
		}
	}

	public static enum InputCommand {
		INIT, GET_GAME_INFO, SET_PLAYER_OUTPUT, SET_PLAYER_TIMEOUT
	}

	public static enum OutputCommand {
		VIEW, INFOS, NEXT_PLAYER_INPUT, NEXT_PLAYER_INFO, SCORES, UINPUT, TOOLTIP, SUMMARY;

		public String format(final int lineCount) {
			return String.format("[[%s] %d]", this.name(), lineCount);
		}
	}

	@SuppressWarnings("serial")
	public static class OutputData extends LinkedList<String> {
		private OutputCommand command;

		public OutputData(final OutputCommand command) {
			this.command = command;
		}

		public boolean add(final String s) {
			if (s != null)
				return super.add(s);
			return false;
		}

		public void addAll(final String[] data) {
			if (data != null)
				super.addAll(Arrays.asList(data));
		}

		@Override
		public String toString() {
			final StringWriter writer = new StringWriter();
			final PrintWriter out = new PrintWriter(writer);
			out.println(this.command.format(this.size()));
			for (final String line : this) {
				out.println(line);
			}
			return writer.toString().trim();
		}
	}

	private static class Tooltip {
		int player;
		String message;

		public Tooltip(final int player, final String message) {
			this.player = player;
			this.message = message;
		}
	}

	private Set<Tooltip> tooltips;
	private int playerCount, alivePlayerCount;
	private int currentPlayer, nextPlayer;
	private PlayerStatus lastPlayer, playerStatus;
	private int frame, round;
	private PlayerStatus[] players;
	private String[] initLines;
	private boolean newRound;
	private String reasonCode, reason;

	private InputStream is;
	private PrintStream out;
	private PrintStream err;

	public AbstractReferee(final InputStream is, final PrintStream out, final PrintStream err) throws IOException {
		this.tooltips = new HashSet<>();
		this.is = is;
		this.out = out;
		this.err = err;
		start();
	}

	@SuppressWarnings("resource")
	public void start() throws IOException {
		try {
			handleInitInputForReferee(2, new String[0]);
		} catch (final InvalidFormatException e) {
			return;
		}

		final Scanner s = new Scanner(this.is);

		try {
			// Read ###Start 2
			s.nextLine();
			this.playerCount = this.alivePlayerCount = 2;
			this.players = new PlayerStatus[2];
			this.players[0] = new PlayerStatus(0);
			this.players[1] = new PlayerStatus(1);
			this.playerStatus = this.players[0];
			this.currentPlayer = this.nextPlayer = 1;
			this.round = -1;
			this.newRound = true;

			for (final PlayerStatus player : this.players) {
				this.lastPlayer = this.playerStatus;
				this.playerStatus = nextPlayer();
				prepare(this.round);
				player.nextInput = getInputForPlayer(this.round, player.id);

				this.out.println("###Input " + this.nextPlayer);
				if (this.round == 0) {
					for (final String line : getInitInputForPlayer(this.nextPlayer)) {
						this.out.println(line);
					}
				}
				for (final String line : this.players[this.nextPlayer].nextInput) {
					this.out.println(line);
				}
			}

			// Reset after displaying the initial state
			this.playerStatus = this.players[0];
			this.currentPlayer = this.nextPlayer = 1;
			this.round = -1;
			this.newRound = true;

			while (true) {
				this.lastPlayer = this.playerStatus;
				this.playerStatus = nextPlayer();

				if (this.round >= getMaxRoundCount(this.playerCount)) {
					throw new GameOverException("maxRoundsCountReached");
				}

				if (this.newRound) {
					prepare(this.round);
					if (!this.isTurnBasedGame()) {
						for (final PlayerStatus player : this.players) {
							if (!player.lost) {
								player.nextInput = getInputForPlayer(this.round, player.id);
							} else {
								player.nextInput = null;
							}
						}
					}
				}

				this.out.println("###Input " + this.nextPlayer);
				for (final String line : this.players[this.nextPlayer].nextInput) {
					this.out.println(line);
				}

				final int expectedOutputLineCount = getExpectedOutputLineCountForPlayer(this.nextPlayer);
				this.out.println("###Output " + this.nextPlayer + " " + expectedOutputLineCount);
				try {
					final String[] outputs = new String[expectedOutputLineCount];
					for (int i = 0; i < expectedOutputLineCount; i++) {
						outputs[i] = s.nextLine();
					}
					handlePlayerOutput(0, this.round, this.nextPlayer, outputs);
				} catch (final WinException e) {
					this.playerStatus.score = getScore(this.nextPlayer);
					this.playerStatus.win = true;
					this.playerStatus.info = e.getReason();
					this.playerStatus.reasonCode = e.getReasonCode();
					this.lastPlayer = this.playerStatus;
					throw new GameOverException(null);
				} catch (final LostException | InvalidInputException e) {
					this.playerStatus.score = getScore(this.nextPlayer);
					this.playerStatus.lost = true;
					this.playerStatus.info = e.getReason();
				}
			}
		} catch (final GameOverException e) {
			this.newRound = true;
			this.reasonCode = e.getReasonCode();
			this.reason = e.getReason();
			this.err.println(this.reason);
			prepare(this.round);
			updateScores();
			if (this.players[0].score > this.players[1].score) {
				this.out.println("###End 0 1");
			} else if (this.players[0].score < this.players[1].score) {
				this.out.println("###End 1 0");
			} else {
				this.out.println("###End 01");
			}
		} finally {
			s.close();
		}
	}

	private PlayerStatus nextPlayer() throws GameOverException {
		// TODO break the loop when all players are lost

		this.currentPlayer = this.nextPlayer;
		this.newRound = false;
		do {
			++this.nextPlayer;
			if (this.nextPlayer >= this.playerCount) {
				nextRound();
				this.nextPlayer = 0;
			}
		} while (this.players[this.nextPlayer].lost || this.players[this.nextPlayer].win);
		return this.players[this.nextPlayer];
	}

	protected String getColoredReason(final boolean error, final String reason) {
		if (error) {
			return String.format("¤RED¤%s§RED§", reason);
		} else {
			return String.format("¤GREEN¤%s§GREEN§", reason);
		}
	}

	private void dumpView() {
		final OutputData data = new OutputData(OutputCommand.VIEW);
		String reasonCode = this.reasonCode;
		if (reasonCode == null && this.playerStatus != null)
			reasonCode = this.playerStatus.reasonCode;

		if (this.newRound) {
			if (reasonCode != null) {
				data.add(String.format("KEY_FRAME %d %s", this.frame, reasonCode));
			} else {
				data.add(String.format("KEY_FRAME %d", this.frame));
			}
			if (this.frame == 0) {
				data.add(getGameName());
				data.addAll(getInitDataForView());
			}
		} else {
			if (reasonCode != null) {
				data.add(String.format("INTERMEDIATE_FRAME %d %s", this.frame, reasonCode));
			} else {
				data.add(String.format("INTERMEDIATE_FRAME %d", this.frame));
			}
		}
		if (this.newRound || isTurnBasedGame()) {
			data.addAll(getFrameDataForView(this.round, this.frame, this.newRound));
		}

		this.out.println(data);
	}

	private void dumpInfos() {
		OutputData data = new OutputData(OutputCommand.INFOS);
		if (this.reason != null && isTurnBasedGame()) {
			data.add(getColoredReason(true, this.reason));
		} else {
			if (this.lastPlayer != null) {
				final String head = this.lastPlayer.info;
				if (head != null) {
					data.add(getColoredReason(this.lastPlayer.lost, head));
				} else {
					if (this.frame > 0) {
						data.addAll(getPlayerActions(this.currentPlayer, this.newRound ? this.round - 1 : this.round));
					}
				}
			}
		}
		this.out.println(data);
		if (this.newRound && this.round >= -1 && this.playerCount > 1) {
			final OutputData summary = new OutputData(OutputCommand.SUMMARY);
			if (this.frame == 0) {
				final String head = getHeadlineAtGameStartForConsole();
				if (head != null) {
					summary.add(head);
				}
			}
			if (this.round >= 0) {
				summary.addAll(getGameSummary(this.round));
			}
			if (!isTurnBasedGame() && this.reason != null) {
				summary.add(getColoredReason(true, this.reason));
			}
			this.out.println(summary);
		}

		if (!this.tooltips.isEmpty() && (this.newRound || isTurnBasedGame())) {
			data = new OutputData(OutputCommand.TOOLTIP);
			for (final Tooltip t : this.tooltips) {
				data.add(t.message);
				data.add(String.valueOf(t.player));
			}
			this.tooltips.clear();
			this.out.println(data);
		}
	}

	private void dumpNextPlayerInfos() {
		final OutputData data = new OutputData(OutputCommand.NEXT_PLAYER_INFO);
		data.add(String.valueOf(this.nextPlayer));
		data.add(String.valueOf(getExpectedOutputLineCountForPlayer(this.nextPlayer)));
		if (this.round == 0) {
			data.add(String.valueOf(getMillisTimeForFirstRound()));
		} else {
			data.add(String.valueOf(getMillisTimeForRound()));
		}
		this.out.println(data);
	}

	private void dumpNextPlayerInput() {
		final OutputData data = new OutputData(OutputCommand.NEXT_PLAYER_INPUT);
		if (this.round == 0) {
			data.addAll(getInitInputForPlayer(this.nextPlayer));
		}
		if (this.isTurnBasedGame()) {
			this.players[this.nextPlayer].nextInput = getInputForPlayer(this.round, this.nextPlayer);
		}
		data.addAll(this.players[this.nextPlayer].nextInput);
		this.out.println(data);
	}

	protected final String translate(final String code, final Object... values) {
		try {
			return String.format((String) this.messages.get(code), values);
		} catch (final NullPointerException e) {
			return code;
		}
	}

	protected final void printError(final Object message) {
		this.err.println(message);
	}

	protected int getMillisTimeForFirstRound() {
		return 1000;
	}

	protected int getMillisTimeForRound() {
		return 150;
	}

	protected int getMaxRoundCount(final int playerCount) {
		return 400;
	}

	private void nextRound() throws GameOverException {
		this.newRound = true;
		if (++this.round > 0) {
			updateGame(this.round);
		}
		if (gameOver()) {
			throw new GameOverException(null);
		}
	}

	protected boolean gameOver() {
		return this.alivePlayerCount < getMinimumPlayerCount();
	}

	private void updateScores() {
		for (int i = 0; i < this.playerCount; ++i) {
			if (!this.players[i].lost && isPlayerDead(i)) {
				this.alivePlayerCount--;
				this.players[i].lost = true;
				this.players[i].info = getDeathReason(i);
				addToolTip(i, this.players[i].info);
			}
			this.players[i].score = getScore(i);
		}
	}

	protected void addToolTip(final int player, final String message) {
		if (showTooltips())
			this.tooltips.add(new Tooltip(player, message));
	}

	/**
	 * Add message (key = reasonCode, value = reason)
	 *
	 * @param p
	 */
	protected abstract void populateMessages(Properties p);

	protected boolean isTurnBasedGame() {
		return false;
	}

	protected abstract void handleInitInputForReferee(int playerCount, String[] init) throws InvalidFormatException;

	protected abstract String[] getInitDataForView();

	protected abstract String[] getFrameDataForView(int round, int frame, boolean keyFrame);

	protected abstract int getExpectedOutputLineCountForPlayer(int playerIdx);

	protected abstract String getGameName();

	protected abstract void appendDataToEnd(PrintStream stream) throws IOException;

	protected abstract void handlePlayerOutput(int frame, int round, int playerIdx, String[] output) throws WinException, LostException, InvalidInputException;

	protected abstract String[] getInitInputForPlayer(int playerIdx);

	protected abstract String[] getInputForPlayer(int round, int playerIdx);

	protected abstract String getHeadlineAtGameStartForConsole();

	protected abstract int getMinimumPlayerCount();

	protected abstract boolean showTooltips();

	/**
	 * @param round
	 * @return scores of all players
	 * @throws GameOverException
	 */
	protected abstract void updateGame(int round) throws GameOverException;

	protected abstract void prepare(int round);

	protected abstract boolean isPlayerDead(int playerIdx);

	protected abstract String getDeathReason(int playerIdx);

	protected abstract int getScore(int playerIdx);

	protected abstract String[] getGameSummary(int round);

	protected abstract String[] getPlayerActions(int playerIdx, int round);

	protected abstract void setPlayerTimeout(int frame, int round, int playerIdx);
}
