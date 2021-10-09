-module(testVect).
-compile(export_all).

run(Sleep, Jitter) ->
	Log = loggyVect:start([john, paul, ringo, george]),
	A = workerVect:start(john, Log, 13, Sleep, Jitter),
	B = workerVect:start(paul, Log, 23, Sleep, Jitter),
	C = workerVect:start(ringo, Log, 36, Sleep, Jitter),
	D = workerVect:start(george, Log, 49, Sleep, Jitter),
	workerVect:peers(A, [B, C, D]),
	workerVect:peers(B, [A, C, D]),
	workerVect:peers(C, [A, B, D]),
	workerVect:peers(D, [A, B, C]),
	timer:sleep(5000),
	loggyVect:stop(Log),
	workerVect:stop(A),
	workerVect:stop(B),
	workerVect:stop(C),
	workerVect:stop(D).
