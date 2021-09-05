function timer(id) {
    const timerDisplay = document.querySelector(id);
    console.log(timerDisplay)

    function displayTime(seconds) {
        const minutes = Math.floor(seconds / 60);
        const remainderSeconds = seconds % 60;
        const display = `${minutes} min. ${remainderSeconds < 10 ? '0' : ''}${remainderSeconds} seg.`;
        timerDisplay.textContent = display;
    }

    let seconds = 0;
    displayTime(seconds);

    countdown = setInterval(() => {
        seconds += 1
        displayTime(seconds);
    }, 1000);
}





